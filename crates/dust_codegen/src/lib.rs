use std::collections::HashMap;
use std::path::PathBuf;

use anyhow::{anyhow, Result};

use cranelift_codegen::isa;
use cranelift_codegen::settings;
use cranelift_codegen::settings::Configurable;

use cranelift_module::{
    DataDescription,
    FuncId,
    Linkage,
    Module,
};

use cranelift_object::{
    ObjectBuilder,
    ObjectModule,
};

use target_lexicon::Triple;

use dust_dir::*;
use dust_semantics::*;

/// Primary codegen entry point
pub struct Codegen {
    module: ObjectModule,
    funcs: HashMap<ProcId, FuncId>,
}

impl Codegen {
    pub fn new() -> Result<Self> {
        // ─────────────────────────────────────────────────────────────
        // Target ISA (FIXES E0308)
        // ─────────────────────────────────────────────────────────────

        let triple = Triple::host();

        let mut flag_builder = settings::builder();
        flag_builder.set("is_pic", "true")?;

        let flags = settings::Flags::new(flag_builder);

        let isa = isa::lookup(triple.clone())
            .map_err(|e| anyhow!("failed to lookup ISA for {}: {}", triple, e))?
            .finish(flags)
            .map_err(|e| anyhow!("failed to finish ISA for {}: {}", triple, e))?;

        let builder = ObjectBuilder::new(
            isa,
            "dust_module".to_string(),
            cranelift_module::default_libcall_names(),
        )?;

        let module = ObjectModule::new(builder);

        Ok(Self {
            module,
            funcs: HashMap::new(),
        })
    }

    /// Declare all procedures before emitting bodies
    pub fn declare_procs(&mut self, dir: &Dir) -> Result<()> {
        for proc in dir.procs.values() {
            let func_id = self.module.declare_function(
                &proc.mangled_name,
                Linkage::Export,
                &proc.signature,
            )?;

            self.funcs.insert(proc.id, func_id);
        }
        Ok(())
    }

    /// Emit all procedure bodies
    pub fn define_procs(&mut self, dir: &Dir) -> Result<()> {
        for proc in dir.procs.values() {
            let func_id = *self
                .funcs
                .get(&proc.id)
                .ok_or_else(|| anyhow!("procedure not declared: {:?}", proc.id))?;

            let mut ctx = self.module.make_context();
            ctx.func = proc.clif_func.clone();

            self.module.define_function(func_id, &mut ctx)?;
            self.module.clear_context(&mut ctx);
        }
        Ok(())
    }

    /// Emit global data objects
    pub fn define_data(&mut self, dir: &Dir) -> Result<()> {
        for data in dir.data.values() {
            let mut desc = DataDescription::new();
            desc.define(data.bytes.clone().into_boxed_slice());

            let data_id = self.module.declare_data(
                &data.name,
                Linkage::Export,
                false,
                false,
            )?;

            self.module.define_data(data_id, &desc)?;
        }
        Ok(())
    }

    /// Finalize and write object file
    pub fn emit_object(self, out_path: PathBuf) -> Result<()> {
        let obj = self.module.finish();

        std::fs::write(out_path, obj.emit()?)?;
        Ok(())
    }
}