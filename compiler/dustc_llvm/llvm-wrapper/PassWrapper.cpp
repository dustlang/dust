#include <stdio.h>

#include <vector>
#include <set>

#include "LLVMWrapper.h"

#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/InitializePasses.h"
#include "llvm/IR/AutoUpgrade.h"
#include "llvm/IR/AssemblyAnnotationWriter.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Object/ObjectFile.h"
#include "llvm/Object/IRObjectFile.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/CBindingWrapping.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/IPO/FunctionImport.h"
#include "llvm/Transforms/Utils/FunctionImportUtils.h"
#include "llvm/LTO/LTO.h"
#include "llvm-c/Transforms/PassManagerBuilder.h"

#include "llvm/Transforms/Instrumentation.h"
#include "llvm/Transforms/Instrumentation/AddressSanitizer.h"
#include "llvm/Support/TimeProfiler.h"
#include "llvm/Transforms/Instrumentation/ThreadSanitizer.h"
#include "llvm/Transforms/Instrumentation/MemorySanitizer.h"
#include "llvm/Transforms/Instrumentation/HWAddressSanitizer.h"
#include "llvm/Transforms/Utils/CanonicalizeAliases.h"
#include "llvm/Transforms/Utils/NameAnonGlobals.h"

using namespace llvm;

typedef struct LLVMOpaquePass *LLVMPassRef;
typedef struct LLVMOpaqueTargetMachine *LLVMTargetMachineRef;

DEFINE_STDCXX_CONVERSION_FUNCTIONS(Pass, LLVMPassRef)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(TargetMachine, LLVMTargetMachineRef)
#if LLVM_VERSION_LT(11, 0)
DEFINE_STDCXX_CONVERSION_FUNCTIONS(PassManagerBuilder,
                                   LLVMPassManagerBuilderRef)
#endif

extern "C" void LLVMInitializePasses() {
  PassRegistry &Registry = *PassRegistry::getPassRegistry();
  initializeCore(Registry);
  initializeCodeGen(Registry);
  initializeScalarOpts(Registry);
  initializeVectorization(Registry);
  initializeIPO(Registry);
  initializeAnalysis(Registry);
  initializeTransformUtils(Registry);
  initializeInstCombine(Registry);
  initializeInstrumentation(Registry);
  initializeTarget(Registry);
}

extern "C" void LLVMTimeTraceProfilerInitialize() {
#if LLVM_VERSION_GE(10, 0)
  timeTraceProfilerInitialize(
      /* TimeTraceGranularity */ 0,
      /* ProcName */ "dustc");
#else
  timeTraceProfilerInitialize();
#endif
}

extern "C" void LLVMTimeTraceProfilerFinish(const char* FileName) {
  StringRef FN(FileName);
  std::error_code EC;
  raw_fd_ostream OS(FN, EC, sys::fs::CD_CreateAlways);

  timeTraceProfilerWrite(OS);
  timeTraceProfilerCleanup();
}

enum class LLVMDustPassKind {
  Other,
  Function,
  Module,
};

static LLVMDustPassKind toDust(PassKind Kind) {
  switch (Kind) {
  case PT_Function:
    return LLVMDustPassKind::Function;
  case PT_Module:
    return LLVMDustPassKind::Module;
  default:
    return LLVMDustPassKind::Other;
  }
}

extern "C" LLVMPassRef LLVMDustFindAndCreatePass(const char *PassName) {
  StringRef SR(PassName);
  PassRegistry *PR = PassRegistry::getPassRegistry();

  const PassInfo *PI = PR->getPassInfo(SR);
  if (PI) {
    return wrap(PI->createPass());
  }
  return nullptr;
}

extern "C" LLVMPassRef LLVMDustCreateAddressSanitizerFunctionPass(bool Recover) {
  const bool CompileKernel = false;
  const bool UseAfterScope = true;

  return wrap(createAddressSanitizerFunctionPass(CompileKernel, Recover, UseAfterScope));
}

extern "C" LLVMPassRef LLVMDustCreateModuleAddressSanitizerPass(bool Recover) {
  const bool CompileKernel = false;

  return wrap(createModuleAddressSanitizerLegacyPassPass(CompileKernel, Recover));
}

extern "C" LLVMPassRef LLVMDustCreateMemorySanitizerPass(int TrackOrigins, bool Recover) {
  const bool CompileKernel = false;

  return wrap(createMemorySanitizerLegacyPassPass(
      MemorySanitizerOptions{TrackOrigins, Recover, CompileKernel}));
}

extern "C" LLVMPassRef LLVMDustCreateThreadSanitizerPass() {
  return wrap(createThreadSanitizerLegacyPassPass());
}

extern "C" LLVMPassRef LLVMDustCreateHWAddressSanitizerPass(bool Recover) {
  const bool CompileKernel = false;

  return wrap(createHWAddressSanitizerLegacyPassPass(CompileKernel, Recover));
}

extern "C" LLVMDustPassKind LLVMDustPassKind(LLVMPassRef DustPass) {
  assert(DustPass);
  Pass *Pass = unwrap(DustPass);
  return toDust(Pass->getPassKind());
}

extern "C" void LLVMDustAddPass(LLVMPassManagerRef PMR, LLVMPassRef DustPass) {
  assert(DustPass);
  Pass *Pass = unwrap(DustPass);
  PassManagerBase *PMB = unwrap(PMR);
  PMB->add(Pass);
}

extern "C"
void LLVMDustPassManagerBuilderPopulateThinLTOPassManager(
  LLVMPassManagerBuilderRef PMBR,
  LLVMPassManagerRef PMR
) {
  unwrap(PMBR)->populateThinLTOPassManager(*unwrap(PMR));
}

extern "C"
void LLVMDustAddLastExtensionPasses(
    LLVMPassManagerBuilderRef PMBR, LLVMPassRef *Passes, size_t NumPasses) {
  auto AddExtensionPasses = [Passes, NumPasses](
      const PassManagerBuilder &Builder, PassManagerBase &PM) {
    for (size_t I = 0; I < NumPasses; I++) {
      PM.add(unwrap(Passes[I]));
    }
  };
  // Add the passes to both of the pre-finalization extension points,
  // so they are run for optimized and non-optimized builds.
  unwrap(PMBR)->addExtension(PassManagerBuilder::EP_OptimizerLast,
                             AddExtensionPasses);
  unwrap(PMBR)->addExtension(PassManagerBuilder::EP_EnabledOnOptLevel0,
                             AddExtensionPasses);
}

#ifdef LLVM_COMPONENT_X86
#define SUBTARGET_X86 SUBTARGET(X86)
#else
#define SUBTARGET_X86
#endif

#ifdef LLVM_COMPONENT_ARM
#define SUBTARGET_ARM SUBTARGET(ARM)
#else
#define SUBTARGET_ARM
#endif

#ifdef LLVM_COMPONENT_AARCH64
#define SUBTARGET_AARCH64 SUBTARGET(AArch64)
#else
#define SUBTARGET_AARCH64
#endif

#ifdef LLVM_COMPONENT_AVR
#define SUBTARGET_AVR SUBTARGET(AVR)
#else
#define SUBTARGET_AVR
#endif

#ifdef LLVM_COMPONENT_MIPS
#define SUBTARGET_MIPS SUBTARGET(Mips)
#else
#define SUBTARGET_MIPS
#endif

#ifdef LLVM_COMPONENT_POWERPC
#define SUBTARGET_PPC SUBTARGET(PPC)
#else
#define SUBTARGET_PPC
#endif

#ifdef LLVM_COMPONENT_SYSTEMZ
#define SUBTARGET_SYSTEMZ SUBTARGET(SystemZ)
#else
#define SUBTARGET_SYSTEMZ
#endif

#ifdef LLVM_COMPONENT_MSP430
#define SUBTARGET_MSP430 SUBTARGET(MSP430)
#else
#define SUBTARGET_MSP430
#endif

#ifdef LLVM_COMPONENT_RISCV
#define SUBTARGET_RISCV SUBTARGET(RISCV)
#else
#define SUBTARGET_RISCV
#endif

#ifdef LLVM_COMPONENT_SPARC
#define SUBTARGET_SPARC SUBTARGET(Sparc)
#else
#define SUBTARGET_SPARC
#endif

#ifdef LLVM_COMPONENT_HEXAGON
#define SUBTARGET_HEXAGON SUBTARGET(Hexagon)
#else
#define SUBTARGET_HEXAGON
#endif

#define GEN_SUBTARGETS                                                         \
  SUBTARGET_X86                                                                \
  SUBTARGET_ARM                                                                \
  SUBTARGET_AARCH64                                                            \
  SUBTARGET_AVR                                                                \
  SUBTARGET_MIPS                                                               \
  SUBTARGET_PPC                                                                \
  SUBTARGET_SYSTEMZ                                                            \
  SUBTARGET_MSP430                                                             \
  SUBTARGET_SPARC                                                              \
  SUBTARGET_HEXAGON                                                            \
  SUBTARGET_RISCV                                                              \

#define SUBTARGET(x)                                                           \
  namespace llvm {                                                             \
  extern const SubtargetFeatureKV x##FeatureKV[];                              \
  extern const SubtargetFeatureKV x##SubTypeKV[];                              \
  }

GEN_SUBTARGETS
#undef SUBTARGET

extern "C" bool LLVMDustHasFeature(LLVMTargetMachineRef TM,
                                   const char *Feature) {
  TargetMachine *Target = unwrap(TM);
  const MCSubtargetInfo *MCInfo = Target->getMCSubtargetInfo();
  return MCInfo->checkFeatures(std::string("+") + Feature);
}

enum class LLVMDustCodeModel {
  Tiny,
  Small,
  Kernel,
  Medium,
  Large,
  None,
};

static Optional<CodeModel::Model> fromDust(LLVMDustCodeModel Model) {
  switch (Model) {
  case LLVMDustCodeModel::Tiny:
    return CodeModel::Tiny;
  case LLVMDustCodeModel::Small:
    return CodeModel::Small;
  case LLVMDustCodeModel::Kernel:
    return CodeModel::Kernel;
  case LLVMDustCodeModel::Medium:
    return CodeModel::Medium;
  case LLVMDustCodeModel::Large:
    return CodeModel::Large;
  case LLVMDustCodeModel::None:
    return None;
  default:
    report_fatal_error("Bad CodeModel.");
  }
}

enum class LLVMDustCodeGenOptLevel {
  None,
  Less,
  Default,
  Aggressive,
};

static CodeGenOpt::Level fromDust(LLVMDustCodeGenOptLevel Level) {
  switch (Level) {
  case LLVMDustCodeGenOptLevel::None:
    return CodeGenOpt::None;
  case LLVMDustCodeGenOptLevel::Less:
    return CodeGenOpt::Less;
  case LLVMDustCodeGenOptLevel::Default:
    return CodeGenOpt::Default;
  case LLVMDustCodeGenOptLevel::Aggressive:
    return CodeGenOpt::Aggressive;
  default:
    report_fatal_error("Bad CodeGenOptLevel.");
  }
}

enum class LLVMDustPassBuilderOptLevel {
  O0,
  O1,
  O2,
  O3,
  Os,
  Oz,
};

static PassBuilder::OptimizationLevel fromDust(LLVMDustPassBuilderOptLevel Level) {
  switch (Level) {
  case LLVMDustPassBuilderOptLevel::O0:
    return PassBuilder::OptimizationLevel::O0;
  case LLVMDustPassBuilderOptLevel::O1:
    return PassBuilder::OptimizationLevel::O1;
  case LLVMDustPassBuilderOptLevel::O2:
    return PassBuilder::OptimizationLevel::O2;
  case LLVMDustPassBuilderOptLevel::O3:
    return PassBuilder::OptimizationLevel::O3;
  case LLVMDustPassBuilderOptLevel::Os:
    return PassBuilder::OptimizationLevel::Os;
  case LLVMDustPassBuilderOptLevel::Oz:
    return PassBuilder::OptimizationLevel::Oz;
  default:
    report_fatal_error("Bad PassBuilderOptLevel.");
  }
}

enum class LLVMDustRelocModel {
  Static,
  PIC,
  DynamicNoPic,
  ROPI,
  RWPI,
  ROPIRWPI,
};

static Reloc::Model fromDust(LLVMDustRelocModel DustReloc) {
  switch (DustReloc) {
  case LLVMDustRelocModel::Static:
    return Reloc::Static;
  case LLVMDustRelocModel::PIC:
    return Reloc::PIC_;
  case LLVMDustRelocModel::DynamicNoPic:
    return Reloc::DynamicNoPIC;
  case LLVMDustRelocModel::ROPI:
    return Reloc::ROPI;
  case LLVMDustRelocModel::RWPI:
    return Reloc::RWPI;
  case LLVMDustRelocModel::ROPIRWPI:
    return Reloc::ROPI_RWPI;
  }
  report_fatal_error("Bad RelocModel.");
}

#ifdef LLVM_DUSTLLVM
/// getLongestEntryLength - Return the length of the longest entry in the table.
template<typename KV>
static size_t getLongestEntryLength(ArrayRef<KV> Table) {
  size_t MaxLen = 0;
  for (auto &I : Table)
    MaxLen = std::max(MaxLen, std::strlen(I.Key));
  return MaxLen;
}

extern "C" void LLVMDustPrintTargetCPUs(LLVMTargetMachineRef TM) {
  const TargetMachine *Target = unwrap(TM);
  const MCSubtargetInfo *MCInfo = Target->getMCSubtargetInfo();
  const Triple::ArchType HostArch = Triple(sys::getProcessTriple()).getArch();
  const Triple::ArchType TargetArch = Target->getTargetTriple().getArch();
  const ArrayRef<SubtargetSubTypeKV> CPUTable = MCInfo->getCPUTable();
  unsigned MaxCPULen = getLongestEntryLength(CPUTable);

  printf("Available CPUs for this target:\n");
  if (HostArch == TargetArch) {
    const StringRef HostCPU = sys::getHostCPUName();
    printf("    %-*s - Select the CPU of the current host (currently %.*s).\n",
      MaxCPULen, "native", (int)HostCPU.size(), HostCPU.data());
  }
  for (auto &CPU : CPUTable)
    printf("    %-*s\n", MaxCPULen, CPU.Key);
  printf("\n");
}

extern "C" void LLVMDustPrintTargetFeatures(LLVMTargetMachineRef TM) {
  const TargetMachine *Target = unwrap(TM);
  const MCSubtargetInfo *MCInfo = Target->getMCSubtargetInfo();
  const ArrayRef<SubtargetFeatureKV> FeatTable = MCInfo->getFeatureTable();
  unsigned MaxFeatLen = getLongestEntryLength(FeatTable);

  printf("Available features for this target:\n");
  for (auto &Feature : FeatTable)
    printf("    %-*s - %s.\n", MaxFeatLen, Feature.Key, Feature.Desc);
  printf("\nDust-specific features:\n");
  printf("    %-*s - %s.\n",
    MaxFeatLen,
    "crt-static",
    "Enables libraries with C Run-time Libraries(CRT) to be statically linked"
  );
  printf("\n");

  printf("Use +feature to enable a feature, or -feature to disable it.\n"
         "For example, dustc -C -target-cpu=mycpu -C "
         "target-feature=+feature1,-feature2\n\n");
}

#else

extern "C" void LLVMDustPrintTargetCPUs(LLVMTargetMachineRef) {
  printf("Target CPU help is not supported by this LLVM version.\n\n");
}

extern "C" void LLVMDustPrintTargetFeatures(LLVMTargetMachineRef) {
  printf("Target features help is not supported by this LLVM version.\n\n");
}
#endif

extern "C" const char* LLVMDustGetHostCPUName(size_t *len) {
  StringRef Name = sys::getHostCPUName();
  *len = Name.size();
  return Name.data();
}

extern "C" LLVMTargetMachineRef LLVMDustCreateTargetMachine(
    const char *TripleStr, const char *CPU, const char *Feature,
    const char *ABIStr, LLVMDustCodeModel DustCM, LLVMDustRelocModel DustReloc,
    LLVMDustCodeGenOptLevel DustOptLevel, bool UseSoftFloat,
    bool FunctionSections,
    bool DataSections,
    bool TrapUnreachable,
    bool Singlethread,
    bool AsmComments,
    bool EmitStackSizeSection,
    bool RelaxELFRelocations,
    bool UseInitArray,
    const char *SplitDwarfFile) {

  auto OptLevel = fromDust(DustOptLevel);
  auto RM = fromDust(DustReloc);
  auto CM = fromDust(DustCM);

  std::string Error;
  Triple Trip(Triple::normalize(TripleStr));
  const llvm::Target *TheTarget =
      TargetRegistry::lookupTarget(Trip.getTriple(), Error);
  if (TheTarget == nullptr) {
    LLVMDustSetLastError(Error.c_str());
    return nullptr;
  }

  TargetOptions Options;

  Options.FloatABIType = FloatABI::Default;
  if (UseSoftFloat) {
    Options.FloatABIType = FloatABI::Soft;
  }
  Options.DataSections = DataSections;
  Options.FunctionSections = FunctionSections;
  Options.MCOptions.AsmVerbose = AsmComments;
  Options.MCOptions.PreserveAsmComments = AsmComments;
  Options.MCOptions.ABIName = ABIStr;
  if (SplitDwarfFile) {
      Options.MCOptions.SplitDwarfFile = SplitDwarfFile;
  }
  Options.RelaxELFRelocations = RelaxELFRelocations;
  Options.UseInitArray = UseInitArray;

  if (TrapUnreachable) {
    // Tell LLVM to codegen `unreachable` into an explicit trap instruction.
    // This limits the extent of possible undefined behavior in some cases, as
    // it prevents control flow from "falling through" into whatever code
    // happens to be laid out next in memory.
    Options.TrapUnreachable = true;
  }

  if (Singlethread) {
    Options.ThreadModel = ThreadModel::Single;
  }

  Options.EmitStackSizeSection = EmitStackSizeSection;

  TargetMachine *TM = TheTarget->createTargetMachine(
      Trip.getTriple(), CPU, Feature, Options, RM, CM, OptLevel);
  return wrap(TM);
}

extern "C" void LLVMDustDisposeTargetMachine(LLVMTargetMachineRef TM) {
  delete unwrap(TM);
}

extern "C" void LLVMDustConfigurePassManagerBuilder(
    LLVMPassManagerBuilderRef PMBR, LLVMDustCodeGenOptLevel OptLevel,
    bool MergeFunctions, bool SLPVectorize, bool LoopVectorize, bool PrepareForThinLTO,
    const char* PGOGenPath, const char* PGOUsePath) {
  unwrap(PMBR)->MergeFunctions = MergeFunctions;
  unwrap(PMBR)->SLPVectorize = SLPVectorize;
  unwrap(PMBR)->OptLevel = fromDust(OptLevel);
  unwrap(PMBR)->LoopVectorize = LoopVectorize;
  unwrap(PMBR)->PrepareForThinLTO = PrepareForThinLTO;

  if (PGOGenPath) {
    assert(!PGOUsePath);
    unwrap(PMBR)->EnablePGOInstrGen = true;
    unwrap(PMBR)->PGOInstrGen = PGOGenPath;
  }
  if (PGOUsePath) {
    assert(!PGOGenPath);
    unwrap(PMBR)->PGOInstrUse = PGOUsePath;
  }
}

// Unfortunately, the LLVM C API doesn't provide a way to set the `LibraryInfo`
// field of a PassManagerBuilder, we expose our own method of doing so.
extern "C" void LLVMDustAddBuilderLibraryInfo(LLVMPassManagerBuilderRef PMBR,
                                              LLVMModuleRef M,
                                              bool DisableSimplifyLibCalls) {
  Triple TargetTriple(unwrap(M)->getTargetTriple());
  TargetLibraryInfoImpl *TLI = new TargetLibraryInfoImpl(TargetTriple);
  if (DisableSimplifyLibCalls)
    TLI->disableAllFunctions();
  unwrap(PMBR)->LibraryInfo = TLI;
}

// Unfortunately, the LLVM C API doesn't provide a way to create the
// TargetLibraryInfo pass, so we use this method to do so.
extern "C" void LLVMDustAddLibraryInfo(LLVMPassManagerRef PMR, LLVMModuleRef M,
                                       bool DisableSimplifyLibCalls) {
  Triple TargetTriple(unwrap(M)->getTargetTriple());
  TargetLibraryInfoImpl TLII(TargetTriple);
  if (DisableSimplifyLibCalls)
    TLII.disableAllFunctions();
  unwrap(PMR)->add(new TargetLibraryInfoWrapperPass(TLII));
}

// Unfortunately, the LLVM C API doesn't provide an easy way of iterating over
// all the functions in a module, so we do that manually here. You'll find
// similar code in clang's BackendUtil.cpp file.
extern "C" void LLVMDustRunFunctionPassManager(LLVMPassManagerRef PMR,
                                               LLVMModuleRef M) {
  llvm::legacy::FunctionPassManager *P =
      unwrap<llvm::legacy::FunctionPassManager>(PMR);
  P->doInitialization();

  // Upgrade all calls to old intrinsics first.
  for (Module::iterator I = unwrap(M)->begin(), E = unwrap(M)->end(); I != E;)
    UpgradeCallsToIntrinsic(&*I++); // must be post-increment, as we remove

  for (Module::iterator I = unwrap(M)->begin(), E = unwrap(M)->end(); I != E;
       ++I)
    if (!I->isDeclaration())
      P->run(*I);

  P->doFinalization();
}

extern "C" void LLVMDustSetLLVMOptions(int Argc, char **Argv) {
  // Initializing the command-line options more than once is not allowed. So,
  // check if they've already been initialized.  (This could happen if we're
  // being called from dustpkg, for example). If the arguments change, then
  // that's just kinda unfortunate.
  static bool Initialized = false;
  if (Initialized)
    return;
  Initialized = true;
  cl::ParseCommandLineOptions(Argc, Argv);
}

enum class LLVMDustFileType {
  AssemblyFile,
  ObjectFile,
};

#if LLVM_VERSION_GE(10, 0)
static CodeGenFileType fromDust(LLVMDustFileType Type) {
  switch (Type) {
  case LLVMDustFileType::AssemblyFile:
    return CGFT_AssemblyFile;
  case LLVMDustFileType::ObjectFile:
    return CGFT_ObjectFile;
  default:
    report_fatal_error("Bad FileType.");
  }
}
#else
static TargetMachine::CodeGenFileType fromDust(LLVMDustFileType Type) {
  switch (Type) {
  case LLVMDustFileType::AssemblyFile:
    return TargetMachine::CGFT_AssemblyFile;
  case LLVMDustFileType::ObjectFile:
    return TargetMachine::CGFT_ObjectFile;
  default:
    report_fatal_error("Bad FileType.");
  }
}
#endif

extern "C" LLVMDustResult
LLVMDustWriteOutputFile(LLVMTargetMachineRef Target, LLVMPassManagerRef PMR,
                        LLVMModuleRef M, const char *Path, const char *DwoPath,
                        LLVMDustFileType DustFileType) {
  llvm::legacy::PassManager *PM = unwrap<llvm::legacy::PassManager>(PMR);
  auto FileType = fromDust(DustFileType);

  std::string ErrorInfo;
  std::error_code EC;
  raw_fd_ostream OS(Path, EC, sys::fs::F_None);
  if (EC)
    ErrorInfo = EC.message();
  if (ErrorInfo != "") {
    LLVMDustSetLastError(ErrorInfo.c_str());
    return LLVMDustResult::Failure;
  }

  buffer_ostream BOS(OS);
  if (DwoPath) {
    raw_fd_ostream DOS(DwoPath, EC, sys::fs::F_None);
    EC.clear();
    if (EC)
        ErrorInfo = EC.message();
    if (ErrorInfo != "") {
      LLVMDustSetLastError(ErrorInfo.c_str());
      return LLVMDustResult::Failure;
    }
    buffer_ostream DBOS(DOS);
    unwrap(Target)->addPassesToEmitFile(*PM, BOS, &DBOS, FileType, false);
    PM->run(*unwrap(M));
  } else {
    unwrap(Target)->addPassesToEmitFile(*PM, BOS, nullptr, FileType, false);
    PM->run(*unwrap(M));
  }

  // Apparently `addPassesToEmitFile` adds a pointer to our on-the-stack output
  // stream (OS), so the only real safe place to delete this is here? Don't we
  // wish this was written in Dust?
  LLVMDisposePassManager(PMR);
  return LLVMDustResult::Success;
}

extern "C" typedef void (*LLVMDustSelfProfileBeforePassCallback)(void*, // LlvmSelfProfiler
                                                      const char*,      // pass name
                                                      const char*);     // IR name
extern "C" typedef void (*LLVMDustSelfProfileAfterPassCallback)(void*); // LlvmSelfProfiler

std::string LLVMDustwrappedIrGetName(const llvm::Any &WrappedIr) {
  if (any_isa<const Module *>(WrappedIr))
    return any_cast<const Module *>(WrappedIr)->getName().str();
  if (any_isa<const Function *>(WrappedIr))
    return any_cast<const Function *>(WrappedIr)->getName().str();
  if (any_isa<const Loop *>(WrappedIr))
    return any_cast<const Loop *>(WrappedIr)->getName().str();
  if (any_isa<const LazyCallGraph::SCC *>(WrappedIr))
    return any_cast<const LazyCallGraph::SCC *>(WrappedIr)->getName();
  return "<UNKNOWN>";
}


void LLVMSelfProfileInitializeCallbacks(
    PassInstrumentationCallbacks& PIC, void* LlvmSelfProfiler,
    LLVMDustSelfProfileBeforePassCallback BeforePassCallback,
    LLVMDustSelfProfileAfterPassCallback AfterPassCallback) {
#if LLVM_VERSION_GE(12, 0)
  PIC.registerBeforeNonSkippedPassCallback([LlvmSelfProfiler, BeforePassCallback](
                                           StringRef Pass, llvm::Any Ir) {
    std::string PassName = Pass.str();
    std::string IrName = LLVMDustwrappedIrGetName(Ir);
    BeforePassCallback(LlvmSelfProfiler, PassName.c_str(), IrName.c_str());
  });

  PIC.registerAfterPassCallback(
      [LlvmSelfProfiler, AfterPassCallback](StringRef Pass, llvm::Any IR,
                                            const PreservedAnalyses &Preserved) {
        AfterPassCallback(LlvmSelfProfiler);
      });

  PIC.registerAfterPassInvalidatedCallback(
      [LlvmSelfProfiler, AfterPassCallback](StringRef Pass, const PreservedAnalyses &Preserved) {
        AfterPassCallback(LlvmSelfProfiler);
      });
#else
  PIC.registerBeforePassCallback([LlvmSelfProfiler, BeforePassCallback](
                                     StringRef Pass, llvm::Any Ir) {
    std::string PassName = Pass.str();
    std::string IrName = LLVMDustwrappedIrGetName(Ir);
    BeforePassCallback(LlvmSelfProfiler, PassName.c_str(), IrName.c_str());
    return true;
  });

  PIC.registerAfterPassCallback(
      [LlvmSelfProfiler, AfterPassCallback](StringRef Pass, llvm::Any Ir) {
        AfterPassCallback(LlvmSelfProfiler);
      });

  PIC.registerAfterPassInvalidatedCallback(
      [LlvmSelfProfiler, AfterPassCallback](StringRef Pass) {
        AfterPassCallback(LlvmSelfProfiler);
      });
#endif

  PIC.registerBeforeAnalysisCallback([LlvmSelfProfiler, BeforePassCallback](
                                         StringRef Pass, llvm::Any Ir) {
    std::string PassName = Pass.str();
    std::string IrName = LLVMDustwrappedIrGetName(Ir);
    BeforePassCallback(LlvmSelfProfiler, PassName.c_str(), IrName.c_str());
  });

  PIC.registerAfterAnalysisCallback(
      [LlvmSelfProfiler, AfterPassCallback](StringRef Pass, llvm::Any Ir) {
        AfterPassCallback(LlvmSelfProfiler);
      });
}

enum class LLVMDustOptStage {
  PreLinkNoLTO,
  PreLinkThinLTO,
  PreLinkFatLTO,
  ThinLTO,
  FatLTO,
};

struct LLVMDustSanitizerOptions {
  bool SanitizeAddress;
  bool SanitizeAddressRecover;
  bool SanitizeMemory;
  bool SanitizeMemoryRecover;
  int  SanitizeMemoryTrackOrigins;
  bool SanitizeThread;
  bool SanitizeHWAddress;
  bool SanitizeHWAddressRecover;
};

extern "C" void
LLVMDustOptimizeWithNewPassManager(
    LLVMModuleRef ModuleRef,
    LLVMTargetMachineRef TMRef,
    LLVMDustPassBuilderOptLevel OptLevelDust,
    LLVMDustOptStage OptStage,
    bool NoPrepopulatePasses, bool VerifyIR, bool UseThinLTOBuffers,
    bool MergeFunctions, bool UnrollLoops, bool SLPVectorize, bool LoopVectorize,
    bool DisableSimplifyLibCalls, bool EmitLifetimeMarkers,
    LLVMDustSanitizerOptions *SanitizerOptions,
    const char *PGOGenPath, const char *PGOUsePath,
    void* LlvmSelfProfiler,
    LLVMDustSelfProfileBeforePassCallback BeforePassCallback,
    LLVMDustSelfProfileAfterPassCallback AfterPassCallback) {
  Module *TheModule = unwrap(ModuleRef);
  TargetMachine *TM = unwrap(TMRef);
  PassBuilder::OptimizationLevel OptLevel = fromDust(OptLevelDust);


  PipelineTuningOptions PTO;
  PTO.LoopUnrolling = UnrollLoops;
  PTO.LoopInterleaving = UnrollLoops;
  PTO.LoopVectorization = LoopVectorize;
  PTO.SLPVectorization = SLPVectorize;
#if LLVM_VERSION_GE(12, 0)
  PTO.MergeFunctions = MergeFunctions;
#else
  // MergeFunctions is not supported by NewPM in older LLVM versions.
  (void) MergeFunctions;
#endif

  // FIXME: We may want to expose this as an option.
  bool DebugPassManager = false;

  PassInstrumentationCallbacks PIC;
#if LLVM_VERSION_GE(12, 0)
  StandardInstrumentations SI(DebugPassManager);
#else
  StandardInstrumentations SI;
#endif
  SI.registerCallbacks(PIC);

  if (LlvmSelfProfiler){
    LLVMSelfProfileInitializeCallbacks(PIC,LlvmSelfProfiler,BeforePassCallback,AfterPassCallback);
  }

  Optional<PGOOptions> PGOOpt;
  if (PGOGenPath) {
    assert(!PGOUsePath);
    PGOOpt = PGOOptions(PGOGenPath, "", "", PGOOptions::IRInstr);
  } else if (PGOUsePath) {
    assert(!PGOGenPath);
    PGOOpt = PGOOptions(PGOUsePath, "", "", PGOOptions::IRUse);
  }

#if LLVM_VERSION_GE(12, 0)
  PassBuilder PB(DebugPassManager, TM, PTO, PGOOpt, &PIC);
#else
  PassBuilder PB(TM, PTO, PGOOpt, &PIC);
#endif

  LoopAnalysisManager LAM(DebugPassManager);
  FunctionAnalysisManager FAM(DebugPassManager);
  CGSCCAnalysisManager CGAM(DebugPassManager);
  ModuleAnalysisManager MAM(DebugPassManager);

  FAM.registerPass([&] { return PB.buildDefaultAAPipeline(); });

  Triple TargetTriple(TheModule->getTargetTriple());
  std::unique_ptr<TargetLibraryInfoImpl> TLII(new TargetLibraryInfoImpl(TargetTriple));
  if (DisableSimplifyLibCalls)
    TLII->disableAllFunctions();
  FAM.registerPass([&] { return TargetLibraryAnalysis(*TLII); });

  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  // We manually collect pipeline callbacks so we can apply them at O0, where the
  // PassBuilder does not create a pipeline.
  std::vector<std::function<void(ModulePassManager &, PassBuilder::OptimizationLevel)>>
      PipelineStartEPCallbacks;
#if LLVM_VERSION_GE(11, 0)
  std::vector<std::function<void(ModulePassManager &, PassBuilder::OptimizationLevel)>>
      OptimizerLastEPCallbacks;
#else
  std::vector<std::function<void(FunctionPassManager &, PassBuilder::OptimizationLevel)>>
      OptimizerLastEPCallbacks;
#endif

  if (VerifyIR) {
    PipelineStartEPCallbacks.push_back(
      [VerifyIR](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
        MPM.addPass(VerifierPass());
      }
    );
  }

  if (SanitizerOptions) {
    if (SanitizerOptions->SanitizeMemory) {
      MemorySanitizerOptions Options(
          SanitizerOptions->SanitizeMemoryTrackOrigins,
          SanitizerOptions->SanitizeMemoryRecover,
          /*CompileKernel=*/false);
#if LLVM_VERSION_GE(11, 0)
      OptimizerLastEPCallbacks.push_back(
        [Options](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(MemorySanitizerPass(Options));
          MPM.addPass(createModuleToFunctionPassAdaptor(MemorySanitizerPass(Options)));
        }
      );
#else
#if LLVM_VERSION_GE(10, 0)
      PipelineStartEPCallbacks.push_back(
        [Options](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(MemorySanitizerPass(Options));
        }
      );
#endif
      OptimizerLastEPCallbacks.push_back(
        [Options](FunctionPassManager &FPM, PassBuilder::OptimizationLevel Level) {
          FPM.addPass(MemorySanitizerPass(Options));
        }
      );
#endif
    }

    if (SanitizerOptions->SanitizeThread) {
#if LLVM_VERSION_GE(11, 0)
      OptimizerLastEPCallbacks.push_back(
        [](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(ThreadSanitizerPass());
          MPM.addPass(createModuleToFunctionPassAdaptor(ThreadSanitizerPass()));
        }
      );
#else
#if LLVM_VERSION_GE(10, 0)
      PipelineStartEPCallbacks.push_back(
        [](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(ThreadSanitizerPass());
        }
      );
#endif
      OptimizerLastEPCallbacks.push_back(
        [](FunctionPassManager &FPM, PassBuilder::OptimizationLevel Level) {
          FPM.addPass(ThreadSanitizerPass());
        }
      );
#endif
    }

    if (SanitizerOptions->SanitizeAddress) {
#if LLVM_VERSION_GE(11, 0)
      OptimizerLastEPCallbacks.push_back(
        [SanitizerOptions](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(RequireAnalysisPass<ASanGlobalsMetadataAnalysis, Module>());
          MPM.addPass(ModuleAddressSanitizerPass(
              /*CompileKernel=*/false, SanitizerOptions->SanitizeAddressRecover));
          MPM.addPass(createModuleToFunctionPassAdaptor(AddressSanitizerPass(
              /*CompileKernel=*/false, SanitizerOptions->SanitizeAddressRecover,
              /*UseAfterScope=*/true)));
        }
      );
#else
      PipelineStartEPCallbacks.push_back(
        [&](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(RequireAnalysisPass<ASanGlobalsMetadataAnalysis, Module>());
        }
      );
      OptimizerLastEPCallbacks.push_back(
        [SanitizerOptions](FunctionPassManager &FPM, PassBuilder::OptimizationLevel Level) {
          FPM.addPass(AddressSanitizerPass(
              /*CompileKernel=*/false, SanitizerOptions->SanitizeAddressRecover,
              /*UseAfterScope=*/true));
        }
      );
      PipelineStartEPCallbacks.push_back(
        [SanitizerOptions](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(ModuleAddressSanitizerPass(
              /*CompileKernel=*/false, SanitizerOptions->SanitizeAddressRecover));
        }
      );
#endif
    }
    if (SanitizerOptions->SanitizeHWAddress) {
#if LLVM_VERSION_GE(11, 0)
      OptimizerLastEPCallbacks.push_back(
        [SanitizerOptions](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(HWAddressSanitizerPass(
              /*CompileKernel=*/false, SanitizerOptions->SanitizeHWAddressRecover));
        }
      );
#else
      PipelineStartEPCallbacks.push_back(
        [SanitizerOptions](ModulePassManager &MPM, PassBuilder::OptimizationLevel Level) {
          MPM.addPass(HWAddressSanitizerPass(
              /*CompileKernel=*/false, SanitizerOptions->SanitizeHWAddressRecover));
        }
      );
#endif
    }
  }

  ModulePassManager MPM(DebugPassManager);
  bool NeedThinLTOBufferPasses = UseThinLTOBuffers;
  if (!NoPrepopulatePasses) {
    if (OptLevel == PassBuilder::OptimizationLevel::O0) {
#if LLVM_VERSION_GE(12, 0)
      for (const auto &C : PipelineStartEPCallbacks)
        PB.registerPipelineStartEPCallback(C);
      for (const auto &C : OptimizerLastEPCallbacks)
        PB.registerOptimizerLastEPCallback(C);

      // Pass false as we manually schedule ThinLTOBufferPasses below.
      MPM = PB.buildO0DefaultPipeline(OptLevel, /* PreLinkLTO */ false);
#else
      for (const auto &C : PipelineStartEPCallbacks)
        C(MPM, OptLevel);

# if LLVM_VERSION_GE(11, 0)
      for (const auto &C : OptimizerLastEPCallbacks)
        C(MPM, OptLevel);
# else
      if (!OptimizerLastEPCallbacks.empty()) {
        FunctionPassManager FPM(DebugPassManager);
        for (const auto &C : OptimizerLastEPCallbacks)
          C(FPM, OptLevel);
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
      }
# endif

      MPM.addPass(AlwaysInlinerPass(EmitLifetimeMarkers));

# if LLVM_VERSION_GE(10, 0)
      if (PGOOpt) {
        PB.addPGOInstrPassesForO0(
            MPM, DebugPassManager, PGOOpt->Action == PGOOptions::IRInstr,
            /*IsCS=*/false, PGOOpt->ProfileFile, PGOOpt->ProfileRemappingFile);
      }
# endif
#endif
    } else {
#if LLVM_VERSION_GE(12, 0)
      for (const auto &C : PipelineStartEPCallbacks)
        PB.registerPipelineStartEPCallback(C);
#else
      for (const auto &C : PipelineStartEPCallbacks)
        PB.registerPipelineStartEPCallback([C, OptLevel](ModulePassManager &MPM) {
          C(MPM, OptLevel);
        });
#endif
      if (OptStage != LLVMDustOptStage::PreLinkThinLTO) {
        for (const auto &C : OptimizerLastEPCallbacks)
          PB.registerOptimizerLastEPCallback(C);
      }

      switch (OptStage) {
      case LLVMDustOptStage::PreLinkNoLTO:
        MPM = PB.buildPerModuleDefaultPipeline(OptLevel, DebugPassManager);
        break;
      case LLVMDustOptStage::PreLinkThinLTO:
#if LLVM_VERSION_GE(12, 0)
        MPM = PB.buildThinLTOPreLinkDefaultPipeline(OptLevel);
        // The ThinLTOPreLink pipeline already includes ThinLTOBuffer passes. However, callback
        // passes may still run afterwards. This means we need to run the buffer passes again.
        // FIXME: In LLVM 13, the ThinLTOPreLink pipeline also runs OptimizerLastEPCallbacks
        // before the RequiredLTOPreLinkPasses, in which case we can remove these hacks.
        if (OptimizerLastEPCallbacks.empty())
          NeedThinLTOBufferPasses = false;
#else
        MPM = PB.buildThinLTOPreLinkDefaultPipeline(OptLevel, DebugPassManager);
#endif
#if LLVM_VERSION_GE(11, 0)
        for (const auto &C : OptimizerLastEPCallbacks)
          C(MPM, OptLevel);
#else
        if (!OptimizerLastEPCallbacks.empty()) {
          FunctionPassManager FPM(DebugPassManager);
          for (const auto &C : OptimizerLastEPCallbacks)
            C(FPM, OptLevel);
          MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        }
#endif
        break;
      case LLVMDustOptStage::PreLinkFatLTO:
#if LLVM_VERSION_GE(12, 0)
        MPM = PB.buildLTOPreLinkDefaultPipeline(OptLevel);
        NeedThinLTOBufferPasses = false;
#else
        MPM = PB.buildLTOPreLinkDefaultPipeline(OptLevel, DebugPassManager);
#endif
        break;
      case LLVMDustOptStage::ThinLTO:
        // FIXME: Does it make sense to pass the ModuleSummaryIndex?
        // It only seems to be needed for C++ specific optimizations.
#if LLVM_VERSION_GE(12, 0)
        MPM = PB.buildThinLTODefaultPipeline(OptLevel, nullptr);
#else
        MPM = PB.buildThinLTODefaultPipeline(OptLevel, DebugPassManager, nullptr);
#endif
        break;
      case LLVMDustOptStage::FatLTO:
#if LLVM_VERSION_GE(12, 0)
        MPM = PB.buildLTODefaultPipeline(OptLevel, nullptr);
#else
        MPM = PB.buildLTODefaultPipeline(OptLevel, DebugPassManager, nullptr);
#endif
        break;
      }
    }
  }

  if (NeedThinLTOBufferPasses) {
    MPM.addPass(CanonicalizeAliasesPass());
    MPM.addPass(NameAnonGlobalPass());
  }

  // Upgrade all calls to old intrinsics first.
  for (Module::iterator I = TheModule->begin(), E = TheModule->end(); I != E;)
    UpgradeCallsToIntrinsic(&*I++); // must be post-increment, as we remove

  MPM.run(*TheModule, MAM);
}

// Callback to demangle function name
// Parameters:
// * name to be demangled
// * name len
// * output buffer
// * output buffer len
// Returns len of demangled string, or 0 if demangle failed.
typedef size_t (*DemangleFn)(const char*, size_t, char*, size_t);


namespace {

class DustAssemblyAnnotationWriter : public AssemblyAnnotationWriter {
  DemangleFn Demangle;
  std::vector<char> Buf;

public:
  DustAssemblyAnnotationWriter(DemangleFn Demangle) : Demangle(Demangle) {}

  // Return empty string if demangle failed
  // or if name does not need to be demangled
  StringRef CallDemangle(StringRef name) {
    if (!Demangle) {
      return StringRef();
    }

    if (Buf.size() < name.size() * 2) {
      // Semangled name usually shorter than mangled,
      // but allocate twice as much memory just in case
      Buf.resize(name.size() * 2);
    }

    auto R = Demangle(name.data(), name.size(), Buf.data(), Buf.size());
    if (!R) {
      // Demangle failed.
      return StringRef();
    }

    auto Demangled = StringRef(Buf.data(), R);
    if (Demangled == name) {
      // Do not print anything if demangled name is equal to mangled.
      return StringRef();
    }

    return Demangled;
  }

  void emitFunctionAnnot(const Function *F,
                         formatted_raw_ostream &OS) override {
    StringRef Demangled = CallDemangle(F->getName());
    if (Demangled.empty()) {
        return;
    }

    OS << "; " << Demangled << "\n";
  }

  void emitInstructionAnnot(const Instruction *I,
                            formatted_raw_ostream &OS) override {
    const char *Name;
    const Value *Value;
    if (const CallInst *CI = dyn_cast<CallInst>(I)) {
      Name = "call";
      Value = CI->getCalledOperand();
    } else if (const InvokeInst* II = dyn_cast<InvokeInst>(I)) {
      Name = "invoke";
      Value = II->getCalledOperand();
    } else {
      // Could demangle more operations, e. g.
      // `store %place, @function`.
      return;
    }

    if (!Value->hasName()) {
      return;
    }

    StringRef Demangled = CallDemangle(Value->getName());
    if (Demangled.empty()) {
      return;
    }

    OS << "; " << Name << " " << Demangled << "\n";
  }
};

} // namespace

extern "C" LLVMDustResult
LLVMDustPrintModule(LLVMModuleRef M, const char *Path, DemangleFn Demangle) {
  std::string ErrorInfo;
  std::error_code EC;
  raw_fd_ostream OS(Path, EC, sys::fs::F_None);
  if (EC)
    ErrorInfo = EC.message();
  if (ErrorInfo != "") {
    LLVMDustSetLastError(ErrorInfo.c_str());
    return LLVMDustResult::Failure;
  }

  DustAssemblyAnnotationWriter AAW(Demangle);
  formatted_raw_ostream FOS(OS);
  unwrap(M)->print(FOS, &AAW);

  return LLVMDustResult::Success;
}

extern "C" void LLVMDustPrintPasses() {
  LLVMInitializePasses();
  struct MyListener : PassRegistrationListener {
    void passEnumerate(const PassInfo *Info) {
      StringRef PassArg = Info->getPassArgument();
      StringRef PassName = Info->getPassName();
      if (!PassArg.empty()) {
        // These unsigned->signed casts could theoretically overflow, but
        // realistically never will (and even if, the result is implementation
        // defined rather plain UB).
        printf("%15.*s - %.*s\n", (int)PassArg.size(), PassArg.data(),
               (int)PassName.size(), PassName.data());
      }
    }
  } Listener;

  PassRegistry *PR = PassRegistry::getPassRegistry();
  PR->enumerateWith(&Listener);
}

extern "C" void LLVMDustAddAlwaysInlinePass(LLVMPassManagerBuilderRef PMBR,
                                            bool AddLifetimes) {
  unwrap(PMBR)->Inliner = llvm::createAlwaysInlinerLegacyPass(AddLifetimes);
}

extern "C" void LLVMDustRunRestrictionPass(LLVMModuleRef M, char **Symbols,
                                           size_t Len) {
  llvm::legacy::PassManager passes;

  auto PreserveFunctions = [=](const GlobalValue &GV) {
    for (size_t I = 0; I < Len; I++) {
      if (GV.getName() == Symbols[I]) {
        return true;
      }
    }
    return false;
  };

  passes.add(llvm::createInternalizePass(PreserveFunctions));

  passes.run(*unwrap(M));
}

extern "C" void LLVMDustMarkAllFunctionsNounwind(LLVMModuleRef M) {
  for (Module::iterator GV = unwrap(M)->begin(), E = unwrap(M)->end(); GV != E;
       ++GV) {
    GV->setDoesNotThrow();
    Function *F = dyn_cast<Function>(GV);
    if (F == nullptr)
      continue;

    for (Function::iterator B = F->begin(), BE = F->end(); B != BE; ++B) {
      for (BasicBlock::iterator I = B->begin(), IE = B->end(); I != IE; ++I) {
        if (isa<InvokeInst>(I)) {
          InvokeInst *CI = cast<InvokeInst>(I);
          CI->setDoesNotThrow();
        }
      }
    }
  }
}

extern "C" void
LLVMDustSetDataLayoutFromTargetMachine(LLVMModuleRef Module,
                                       LLVMTargetMachineRef TMR) {
  TargetMachine *Target = unwrap(TMR);
  unwrap(Module)->setDataLayout(Target->createDataLayout());
}

extern "C" void LLVMDustSetModulePICLevel(LLVMModuleRef M) {
  unwrap(M)->setPICLevel(PICLevel::Level::BigPIC);
}

extern "C" void LLVMDustSetModulePIELevel(LLVMModuleRef M) {
  unwrap(M)->setPIELevel(PIELevel::Level::Large);
}

extern "C" void LLVMDustSetModuleCodeModel(LLVMModuleRef M,
                                           LLVMDustCodeModel Model) {
  auto CM = fromDust(Model);
  if (!CM.hasValue())
    return;
  unwrap(M)->setCodeModel(*CM);
}

// Here you'll find an implementation of ThinLTO as used by the Dust compiler
// right now. This ThinLTO support is only enabled on "recent ish" versions of
// LLVM, and otherwise it's just blanket rejected from other compilers.
//
// Most of this implementation is straight copied from LLVM. At the time of
// this writing it wasn't *quite* suitable to reuse more code from upstream
// for our purposes, but we should strive to upstream this support once it's
// ready to go! I figure we may want a bit of testing locally first before
// sending this upstream to LLVM. I hear though they're quite eager to receive
// feedback like this!
//
// If you're reading this code and wondering "what in the world" or you're
// working "good lord by LLVM upgrade is *still* failing due to these bindings"
// then fear not! (ok maybe fear a little). All code here is mostly based
// on `lib/LTO/ThinLTOCodeGenerator.cpp` in LLVM.
//
// You'll find that the general layout here roughly corresponds to the `run`
// method in that file as well as `ProcessThinLTOModule`. Functions are
// specifically commented below as well, but if you're updating this code
// or otherwise trying to understand it, the LLVM source will be useful in
// interpreting the mysteries within.
//
// Otherwise I'll apologize in advance, it probably requires a relatively
// significant investment on your part to "truly understand" what's going on
// here. Not saying I do myself, but it took me awhile staring at LLVM's source
// and various online resources about ThinLTO to make heads or tails of all
// this.

// This is a shared data structure which *must* be threadsafe to share
// read-only amongst threads. This also corresponds basically to the arguments
// of the `ProcessThinLTOModule` function in the LLVM source.
struct LLVMDustThinLTOData {
  // The combined index that is the global analysis over all modules we're
  // performing ThinLTO for. This is mostly managed by LLVM.
  ModuleSummaryIndex Index;

  // All modules we may look at, stored as in-memory serialized versions. This
  // is later used when inlining to ensure we can extract any module to inline
  // from.
  StringMap<MemoryBufferRef> ModuleMap;

  // A set that we manage of everything we *don't* want internalized. Note that
  // this includes all transitive references right now as well, but it may not
  // always!
  DenseSet<GlobalValue::GUID> GUIDPreservedSymbols;

  // Not 100% sure what these are, but they impact what's internalized and
  // what's inlined across modules, I believe.
  StringMap<FunctionImporter::ImportMapTy> ImportLists;
  StringMap<FunctionImporter::ExportSetTy> ExportLists;
  StringMap<GVSummaryMapTy> ModuleToDefinedGVSummaries;
  StringMap<std::map<GlobalValue::GUID, GlobalValue::LinkageTypes>> ResolvedODR;

  LLVMDustThinLTOData() : Index(/* HaveGVs = */ false) {}
};

// Just an argument to the `LLVMDustCreateThinLTOData` function below.
struct LLVMDustThinLTOModule {
  const char *identifier;
  const char *data;
  size_t len;
};

// This is copied from `lib/LTO/ThinLTOCodeGenerator.cpp`, not sure what it
// does.
static const GlobalValueSummary *
getFirstDefinitionForLinker(const GlobalValueSummaryList &GVSummaryList) {
  auto StrongDefForLinker = llvm::find_if(
      GVSummaryList, [](const std::unique_ptr<GlobalValueSummary> &Summary) {
        auto Linkage = Summary->linkage();
        return !GlobalValue::isAvailableExternallyLinkage(Linkage) &&
               !GlobalValue::isWeakForLinker(Linkage);
      });
  if (StrongDefForLinker != GVSummaryList.end())
    return StrongDefForLinker->get();

  auto FirstDefForLinker = llvm::find_if(
      GVSummaryList, [](const std::unique_ptr<GlobalValueSummary> &Summary) {
        auto Linkage = Summary->linkage();
        return !GlobalValue::isAvailableExternallyLinkage(Linkage);
      });
  if (FirstDefForLinker == GVSummaryList.end())
    return nullptr;
  return FirstDefForLinker->get();
}

// The main entry point for creating the global ThinLTO analysis. The structure
// here is basically the same as before threads are spawned in the `run`
// function of `lib/LTO/ThinLTOCodeGenerator.cpp`.
extern "C" LLVMDustThinLTOData*
LLVMDustCreateThinLTOData(LLVMDustThinLTOModule *modules,
                          int num_modules,
                          const char **preserved_symbols,
                          int num_symbols) {
#if LLVM_VERSION_GE(10, 0)
  auto Ret = std::make_unique<LLVMDustThinLTOData>();
#else
  auto Ret = llvm::make_unique<LLVMDustThinLTOData>();
#endif

  // Load each module's summary and merge it into one combined index
  for (int i = 0; i < num_modules; i++) {
    auto module = &modules[i];
    StringRef buffer(module->data, module->len);
    MemoryBufferRef mem_buffer(buffer, module->identifier);

    Ret->ModuleMap[module->identifier] = mem_buffer;

    if (Error Err = readModuleSummaryIndex(mem_buffer, Ret->Index, i)) {
      LLVMDustSetLastError(toString(std::move(Err)).c_str());
      return nullptr;
    }
  }

  // Collect for each module the list of function it defines (GUID -> Summary)
  Ret->Index.collectDefinedGVSummariesPerModule(Ret->ModuleToDefinedGVSummaries);

  // Convert the preserved symbols set from string to GUID, this is then needed
  // for internalization.
  for (int i = 0; i < num_symbols; i++) {
    auto GUID = GlobalValue::getGUID(preserved_symbols[i]);
    Ret->GUIDPreservedSymbols.insert(GUID);
  }

  // Collect the import/export lists for all modules from the call-graph in the
  // combined index
  //
  // This is copied from `lib/LTO/ThinLTOCodeGenerator.cpp`
  auto deadIsPrevailing = [&](GlobalValue::GUID G) {
    return PrevailingType::Unknown;
  };
  // We don't have a complete picture in our use of ThinLTO, just our immediate
  // crate, so we need `ImportEnabled = false` to limit internalization.
  // Otherwise, we sometimes lose `static` values -- see #60184.
  computeDeadSymbolsWithConstProp(Ret->Index, Ret->GUIDPreservedSymbols,
                                  deadIsPrevailing, /* ImportEnabled = */ false);
  ComputeCrossModuleImport(
    Ret->Index,
    Ret->ModuleToDefinedGVSummaries,
    Ret->ImportLists,
    Ret->ExportLists
  );

  // Resolve LinkOnce/Weak symbols, this has to be computed early be cause it
  // impacts the caching.
  //
  // This is copied from `lib/LTO/ThinLTOCodeGenerator.cpp` with some of this
  // being lifted from `lib/LTO/LTO.cpp` as well
  DenseMap<GlobalValue::GUID, const GlobalValueSummary *> PrevailingCopy;
  for (auto &I : Ret->Index) {
    if (I.second.SummaryList.size() > 1)
      PrevailingCopy[I.first] = getFirstDefinitionForLinker(I.second.SummaryList);
  }
  auto isPrevailing = [&](GlobalValue::GUID GUID, const GlobalValueSummary *S) {
    const auto &Prevailing = PrevailingCopy.find(GUID);
    if (Prevailing == PrevailingCopy.end())
      return true;
    return Prevailing->second == S;
  };
  auto recordNewLinkage = [&](StringRef ModuleIdentifier,
                              GlobalValue::GUID GUID,
                              GlobalValue::LinkageTypes NewLinkage) {
    Ret->ResolvedODR[ModuleIdentifier][GUID] = NewLinkage;
  };

  thinLTOResolvePrevailingInIndex(Ret->Index, isPrevailing, recordNewLinkage,
                                  Ret->GUIDPreservedSymbols);

  // Here we calculate an `ExportedGUIDs` set for use in the `isExported`
  // callback below. This callback below will dictate the linkage for all
  // summaries in the index, and we basically just only want to ensure that dead
  // symbols are internalized. Otherwise everything that's already external
  // linkage will stay as external, and internal will stay as internal.
  std::set<GlobalValue::GUID> ExportedGUIDs;
  for (auto &List : Ret->Index) {
    for (auto &GVS: List.second.SummaryList) {
      if (GlobalValue::isLocalLinkage(GVS->linkage()))
        continue;
      auto GUID = GVS->getOriginalName();
      if (GVS->flags().Live)
        ExportedGUIDs.insert(GUID);
    }
  }
#if LLVM_VERSION_GE(10, 0)
  auto isExported = [&](StringRef ModuleIdentifier, ValueInfo VI) {
    const auto &ExportList = Ret->ExportLists.find(ModuleIdentifier);
    return (ExportList != Ret->ExportLists.end() &&
      ExportList->second.count(VI)) ||
      ExportedGUIDs.count(VI.getGUID());
  };
  thinLTOInternalizeAndPromoteInIndex(Ret->Index, isExported, isPrevailing);
#else
  auto isExported = [&](StringRef ModuleIdentifier, GlobalValue::GUID GUID) {
    const auto &ExportList = Ret->ExportLists.find(ModuleIdentifier);
    return (ExportList != Ret->ExportLists.end() &&
      ExportList->second.count(GUID)) ||
      ExportedGUIDs.count(GUID);
  };
  thinLTOInternalizeAndPromoteInIndex(Ret->Index, isExported);
#endif

  return Ret.release();
}

extern "C" void
LLVMDustFreeThinLTOData(LLVMDustThinLTOData *Data) {
  delete Data;
}

// Below are the various passes that happen *per module* when doing ThinLTO.
//
// In other words, these are the functions that are all run concurrently
// with one another, one per module. The passes here correspond to the analysis
// passes in `lib/LTO/ThinLTOCodeGenerator.cpp`, currently found in the
// `ProcessThinLTOModule` function. Here they're split up into separate steps
// so dustc can save off the intermediate bytecode between each step.

#if LLVM_VERSION_GE(11, 0)
static bool
clearDSOLocalOnDeclarations(Module &Mod, TargetMachine &TM) {
  // When linking an ELF shared object, dso_local should be dropped. We
  // conservatively do this for -fpic.
  bool ClearDSOLocalOnDeclarations =
      TM.getTargetTriple().isOSBinFormatELF() &&
      TM.getRelocationModel() != Reloc::Static &&
      Mod.getPIELevel() == PIELevel::Default;
  return ClearDSOLocalOnDeclarations;
}
#endif

extern "C" bool
LLVMDustPrepareThinLTORename(const LLVMDustThinLTOData *Data, LLVMModuleRef M,
                             LLVMTargetMachineRef TM) {
  Module &Mod = *unwrap(M);
  TargetMachine &Target = *unwrap(TM);

#if LLVM_VERSION_GE(11, 0)
  bool ClearDSOLocal = clearDSOLocalOnDeclarations(Mod, Target);
  bool error = renameModuleForThinLTO(Mod, Data->Index, ClearDSOLocal);
#else
  bool error = renameModuleForThinLTO(Mod, Data->Index);
#endif

  if (error) {
    LLVMDustSetLastError("renameModuleForThinLTO failed");
    return false;
  }
  return true;
}

extern "C" bool
LLVMDustPrepareThinLTOResolveWeak(const LLVMDustThinLTOData *Data, LLVMModuleRef M) {
  Module &Mod = *unwrap(M);
  const auto &DefinedGlobals = Data->ModuleToDefinedGVSummaries.lookup(Mod.getModuleIdentifier());
  thinLTOResolvePrevailingInModule(Mod, DefinedGlobals);
  return true;
}

extern "C" bool
LLVMDustPrepareThinLTOInternalize(const LLVMDustThinLTOData *Data, LLVMModuleRef M) {
  Module &Mod = *unwrap(M);
  const auto &DefinedGlobals = Data->ModuleToDefinedGVSummaries.lookup(Mod.getModuleIdentifier());
  thinLTOInternalizeModule(Mod, DefinedGlobals);
  return true;
}

extern "C" bool
LLVMDustPrepareThinLTOImport(const LLVMDustThinLTOData *Data, LLVMModuleRef M,
                             LLVMTargetMachineRef TM) {
  Module &Mod = *unwrap(M);
  TargetMachine &Target = *unwrap(TM);

  const auto &ImportList = Data->ImportLists.lookup(Mod.getModuleIdentifier());
  auto Loader = [&](StringRef Identifier) {
    const auto &Memory = Data->ModuleMap.lookup(Identifier);
    auto &Context = Mod.getContext();
    auto MOrErr = getLazyBitcodeModule(Memory, Context, true, true);

    if (!MOrErr)
      return MOrErr;

    // The rest of this closure is a workaround for
    // https://bugs.llvm.org/show_bug.cgi?id=38184 where during ThinLTO imports
    // we accidentally import wasm custom sections into different modules,
    // duplicating them by in the final output artifact.
    //
    // The issue is worked around here by manually removing the
    // `wasm.custom_sections` named metadata node from any imported module. This
    // we know isn't used by any optimization pass so there's no need for it to
    // be imported.
    //
    // Note that the metadata is currently lazily loaded, so we materialize it
    // here before looking up if there's metadata inside. The `FunctionImporter`
    // will immediately materialize metadata anyway after an import, so this
    // shouldn't be a perf hit.
    if (Error Err = (*MOrErr)->materializeMetadata()) {
      Expected<std::unique_ptr<Module>> Ret(std::move(Err));
      return Ret;
    }

    auto *WasmCustomSections = (*MOrErr)->getNamedMetadata("wasm.custom_sections");
    if (WasmCustomSections)
      WasmCustomSections->eraseFromParent();

    return MOrErr;
  };
#if LLVM_VERSION_GE(11, 0)
  bool ClearDSOLocal = clearDSOLocalOnDeclarations(Mod, Target);
  FunctionImporter Importer(Data->Index, Loader, ClearDSOLocal);
#else
  FunctionImporter Importer(Data->Index, Loader);
#endif
  Expected<bool> Result = Importer.importFunctions(Mod, ImportList);
  if (!Result) {
    LLVMDustSetLastError(toString(Result.takeError()).c_str());
    return false;
  }
  return true;
}

extern "C" typedef void (*LLVMDustModuleNameCallback)(void*, // payload
                                                      const char*, // importing module name
                                                      const char*); // imported module name

// Calls `module_name_callback` for each module import done by ThinLTO.
// The callback is provided with regular null-terminated C strings.
extern "C" void
LLVMDustGetThinLTOModules(const LLVMDustThinLTOData *data,
                                LLVMDustModuleNameCallback module_name_callback,
                                void* callback_payload) {
  for (const auto& importing_module : data->ImportLists) {
    const std::string importing_module_id = importing_module.getKey().str();
    const auto& imports = importing_module.getValue();
    for (const auto& imported_module : imports) {
      const std::string imported_module_id = imported_module.getKey().str();
      module_name_callback(callback_payload,
                           importing_module_id.c_str(),
                           imported_module_id.c_str());
    }
  }
}

// This struct and various functions are sort of a hack right now, but the
// problem is that we've got in-memory LLVM modules after we generate and
// optimize all codegen-units for one compilation in dustc. To be compatible
// with the LTO support above we need to serialize the modules plus their
// ThinLTO summary into memory.
//
// This structure is basically an owned version of a serialize module, with
// a ThinLTO summary attached.
struct LLVMDustThinLTOBuffer {
  std::string data;
};

extern "C" LLVMDustThinLTOBuffer*
LLVMDustThinLTOBufferCreate(LLVMModuleRef M) {
#if LLVM_VERSION_GE(10, 0)
  auto Ret = std::make_unique<LLVMDustThinLTOBuffer>();
#else
  auto Ret = llvm::make_unique<LLVMDustThinLTOBuffer>();
#endif
  {
    raw_string_ostream OS(Ret->data);
    {
      legacy::PassManager PM;
      PM.add(createWriteThinLTOBitcodePass(OS));
      PM.run(*unwrap(M));
    }
  }
  return Ret.release();
}

extern "C" void
LLVMDustThinLTOBufferFree(LLVMDustThinLTOBuffer *Buffer) {
  delete Buffer;
}

extern "C" const void*
LLVMDustThinLTOBufferPtr(const LLVMDustThinLTOBuffer *Buffer) {
  return Buffer->data.data();
}

extern "C" size_t
LLVMDustThinLTOBufferLen(const LLVMDustThinLTOBuffer *Buffer) {
  return Buffer->data.length();
}

// This is what we used to parse upstream bitcode for actual ThinLTO
// processing.  We'll call this once per module optimized through ThinLTO, and
// it'll be called concurrently on many threads.
extern "C" LLVMModuleRef
LLVMDustParseBitcodeForLTO(LLVMContextRef Context,
                           const char *data,
                           size_t len,
                           const char *identifier) {
  StringRef Data(data, len);
  MemoryBufferRef Buffer(Data, identifier);
  unwrap(Context)->enableDebugTypeODRUniquing();
  Expected<std::unique_ptr<Module>> SrcOrError =
      parseBitcodeFile(Buffer, *unwrap(Context));
  if (!SrcOrError) {
    LLVMDustSetLastError(toString(SrcOrError.takeError()).c_str());
    return nullptr;
  }
  return wrap(std::move(*SrcOrError).release());
}

// Find the bitcode section in the object file data and return it as a slice.
// Fail if the bitcode section is present but empty.
//
// On success, the return value is the pointer to the start of the slice and
// `out_len` is filled with the (non-zero) length. On failure, the return value
// is `nullptr` and `out_len` is set to zero.
extern "C" const char*
LLVMDustGetBitcodeSliceFromObjectData(const char *data,
                                      size_t len,
                                      size_t *out_len) {
  *out_len = 0;

  StringRef Data(data, len);
  MemoryBufferRef Buffer(Data, ""); // The id is unused.

  Expected<MemoryBufferRef> BitcodeOrError =
    object::IRObjectFile::findBitcodeInMemBuffer(Buffer);
  if (!BitcodeOrError) {
    LLVMDustSetLastError(toString(BitcodeOrError.takeError()).c_str());
    return nullptr;
  }

  *out_len = BitcodeOrError->getBufferSize();
  return BitcodeOrError->getBufferStart();
}

// Rewrite all `DICompileUnit` pointers to the `DICompileUnit` specified. See
// the comment in `back/lto.rs` for why this exists.
extern "C" void
LLVMDustThinLTOGetDICompileUnit(LLVMModuleRef Mod,
                                DICompileUnit **A,
                                DICompileUnit **B) {
  Module *M = unwrap(Mod);
  DICompileUnit **Cur = A;
  DICompileUnit **Next = B;
  for (DICompileUnit *CU : M->debug_compile_units()) {
    *Cur = CU;
    Cur = Next;
    Next = nullptr;
    if (Cur == nullptr)
      break;
  }
}

// Rewrite all `DICompileUnit` pointers to the `DICompileUnit` specified. See
// the comment in `back/lto.rs` for why this exists.
extern "C" void
LLVMDustThinLTOPatchDICompileUnit(LLVMModuleRef Mod, DICompileUnit *Unit) {
  Module *M = unwrap(Mod);

  // If the original source module didn't have a `DICompileUnit` then try to
  // merge all the existing compile units. If there aren't actually any though
  // then there's not much for us to do so return.
  if (Unit == nullptr) {
    for (DICompileUnit *CU : M->debug_compile_units()) {
      Unit = CU;
      break;
    }
    if (Unit == nullptr)
      return;
  }

  // Use LLVM's built-in `DebugInfoFinder` to find a bunch of debuginfo and
  // process it recursively. Note that we used to specifically iterate over
  // instructions to ensure we feed everything into it, but `processModule`
  // started doing this the same way in LLVM 7 (commit d769eb36ab2b8).
  DebugInfoFinder Finder;
  Finder.processModule(*M);

  // After we've found all our debuginfo, rewrite all subprograms to point to
  // the same `DICompileUnit`.
  for (auto &F : Finder.subprograms()) {
    F->replaceUnit(Unit);
  }

  // Erase any other references to other `DICompileUnit` instances, the verifier
  // will later ensure that we don't actually have any other stale references to
  // worry about.
  auto *MD = M->getNamedMetadata("llvm.dbg.cu");
  MD->clearOperands();
  MD->addOperand(Unit);
}

// Computes the LTO cache key for the provided 'ModId' in the given 'Data',
// storing the result in 'KeyOut'.
// Currently, this cache key is a SHA-1 hash of anything that could affect
// the result of optimizing this module (e.g. module imports, exports, liveness
// of access globals, etc).
// The precise details are determined by LLVM in `computeLTOCacheKey`, which is
// used during the normal linker-plugin incremental thin-LTO process.
extern "C" void
LLVMDustComputeLTOCacheKey(DustStringRef KeyOut, const char *ModId, LLVMDustThinLTOData *Data) {
  SmallString<40> Key;
  llvm::lto::Config conf;
  const auto &ImportList = Data->ImportLists.lookup(ModId);
  const auto &ExportList = Data->ExportLists.lookup(ModId);
  const auto &ResolvedODR = Data->ResolvedODR.lookup(ModId);
  const auto &DefinedGlobals = Data->ModuleToDefinedGVSummaries.lookup(ModId);
  std::set<GlobalValue::GUID> CfiFunctionDefs;
  std::set<GlobalValue::GUID> CfiFunctionDecls;

  // Based on the 'InProcessThinBackend' constructor in LLVM
  for (auto &Name : Data->Index.cfiFunctionDefs())
    CfiFunctionDefs.insert(
        GlobalValue::getGUID(GlobalValue::dropLLVMManglingEscape(Name)));
  for (auto &Name : Data->Index.cfiFunctionDecls())
    CfiFunctionDecls.insert(
        GlobalValue::getGUID(GlobalValue::dropLLVMManglingEscape(Name)));

  llvm::computeLTOCacheKey(Key, conf, Data->Index, ModId,
      ImportList, ExportList, ResolvedODR, DefinedGlobals, CfiFunctionDefs, CfiFunctionDecls
  );

  LLVMDustStringWriteImpl(KeyOut, Key.c_str(), Key.size());
}
