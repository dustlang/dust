BITS 32
ORG 0x10000

%define VGA_BASE      0xB8000
%define VGA_LIMIT     0xB8FA0
%define ATTR          0x0F
%define LINE_MAX      256
%define NAME_MAX      24
%define CUSTOM_MAX    32

%define CWD_ROOT      0
%define CWD_CORE      1
%define CWD_UTILS     2
%define CWD_SHELL     3
%define CWD_EDX       4
%define CWD_CUSTOM    5

%define ENTRY_DIR     1
%define ENTRY_FILE    2

start:
    cli
    call disable_cursor
    call clear_screen
    call init_state

    ; Patched by Rust codegen to absolute address of:
    ; [u32 count][u32 ptr_0..ptr_n-1][str0\0str1\0...]
    mov esi, 0xAAAAAAAA
    mov ecx, [esi]
    add esi, 4
.print_emit:
    test ecx, ecx
    jz .emit_done
    lodsd
    push ecx
    push esi
    mov esi, eax
    call puts
    call newline
    pop esi
    pop ecx
    dec ecx
    jmp .print_emit
.emit_done:
    call newline

shell_loop:
    mov esi, prompt
    call puts
    call read_line
    call parse_line
    call dispatch
    jmp shell_loop

; ------------------------------------------------------------
; Console
; ------------------------------------------------------------

disable_cursor:
    mov dx, 0x3D4
    mov al, 0x0A
    out dx, al
    mov dx, 0x3D5
    in al, dx
    or al, 0x20
    out dx, al
    ret

clear_screen:
    push eax
    push ecx
    push edi
    mov edi, VGA_BASE
    mov ecx, 2000
    mov ax, (ATTR << 8) | 0x20
    rep stosw
    mov dword [cursor], VGA_BASE
    pop edi
    pop ecx
    pop eax
    ret

newline:
    push eax
    push ebx
    push edx
    mov eax, [cursor]
    sub eax, VGA_BASE
    xor edx, edx
    mov ebx, 160
    div ebx
    inc eax
    cmp eax, 25
    jb .set_row
    pop edx
    pop ebx
    pop eax
    call clear_screen
    ret
.set_row:
    imul eax, eax, 160
    add eax, VGA_BASE
    mov [cursor], eax
    pop edx
    pop ebx
    pop eax
    ret

putc:
    cmp al, 13
    je .done
    cmp al, 10
    je .nl
    push eax
    push edi
    mov edi, [cursor]
    mov ah, ATTR
    mov [edi], ax
    add edi, 2
    cmp edi, VGA_LIMIT
    jb .store
    pop edi
    pop eax
    call clear_screen
    ret
.store:
    mov [cursor], edi
    pop edi
    pop eax
    ret
.nl:
    call newline
.done:
    ret

puts:
.loop:
    lodsb
    test al, al
    jz .done
    call putc
    jmp .loop
.done:
    ret

print_line:
    call puts
    call newline
    ret

; print a space-delimited string as one token per line.
; in: ESI -> "token1 token2 token3"
print_space_list:
    push eax
    push esi
.loop:
    mov al, [esi]
    test al, al
    jz .done
    cmp al, ' '
    je .sep
    call putc
    inc esi
    jmp .loop
.sep:
    call newline
.skip:
    inc esi
    cmp byte [esi], ' '
    je .skip
    jmp .loop
.done:
    call newline
    pop esi
    pop eax
    ret

; ------------------------------------------------------------
; Keyboard (US layout, set-1)
; ------------------------------------------------------------

read_key_ascii:
.wait:
    in al, 0x64
    test al, 0x01
    jz .wait
    in al, 0x60
    cmp al, 0xE0
    je .set_ext
    mov bl, al
    mov byte [kb_ext], 0

    test bl, 0x80
    jnz .break

    cmp bl, 0x2A
    je .shift_on
    cmp bl, 0x36
    je .shift_on
    cmp bl, 0x1D
    je .ctrl_on
    cmp bl, 0x38
    je .alt_on
    cmp bl, 0x3A
    je .caps_toggle
    cmp bl, 0x1C
    je .enter
    cmp bl, 0x0E
    je .backspace
    cmp bl, 0x39
    je .space
    cmp bl, 0x0F
    je .tab
    cmp bl, 0x80
    jae .none

    movzx ebx, bl
    mov al, [map_unshift + ebx]
    test al, al
    jz .none

    cmp al, 'a'
    jb .symbol
    cmp al, 'z'
    ja .symbol
    mov ah, [kb_shift]
    xor ah, [kb_caps]
    test ah, 1
    jz .ret
    sub al, 32
    ret

.symbol:
    mov ah, [kb_shift]
    test ah, 1
    jz .ret
    mov al, [map_shift + ebx]
    test al, al
    jz .none
.ret:
    ret

.break:
    and bl, 0x7F
    cmp bl, 0x2A
    je .shift_off
    cmp bl, 0x36
    je .shift_off
    cmp bl, 0x1D
    je .ctrl_off
    cmp bl, 0x38
    je .alt_off
    jmp .none

.set_ext:
    mov byte [kb_ext], 1
    jmp .none
.shift_on:
    mov byte [kb_shift], 1
    jmp .none
.shift_off:
    mov byte [kb_shift], 0
    jmp .none
.ctrl_on:
    mov byte [kb_ctrl], 1
    jmp .none
.ctrl_off:
    mov byte [kb_ctrl], 0
    jmp .none
.alt_on:
    mov byte [kb_alt], 1
    jmp .none
.alt_off:
    mov byte [kb_alt], 0
    jmp .none
.caps_toggle:
    mov al, [kb_caps]
    xor al, 1
    mov [kb_caps], al
    jmp .none
.enter:
    mov al, 10
    ret
.backspace:
    mov al, 8
    ret
.space:
    mov al, ' '
    ret
.tab:
    mov al, ' '
    ret
.none:
    xor al, al
    ret

read_line:
    mov dword [line_len], 0
.next:
    call read_key_ascii
    test al, al
    jz .next
    cmp al, 10
    je .done
    cmp al, 8
    je .backspace

    mov ecx, [line_len]
    cmp ecx, LINE_MAX - 1
    jae .next
    mov [line_buf + ecx], al
    inc ecx
    mov [line_len], ecx
    call putc
    jmp .next

.backspace:
    mov ecx, [line_len]
    test ecx, ecx
    jz .next
    dec ecx
    mov [line_len], ecx
    mov edi, [cursor]
    cmp edi, VGA_BASE
    jbe .next
    sub edi, 2
    mov ax, (ATTR << 8) | 0x20
    mov [edi], ax
    mov [cursor], edi
    jmp .next

.done:
    mov ecx, [line_len]
    mov byte [line_buf + ecx], 0
    call newline
    ret

; ------------------------------------------------------------
; Parsing
; ------------------------------------------------------------

skip_spaces:
    mov al, [esi]
.loop:
    cmp al, ' '
    jne .done
    inc esi
    mov al, [esi]
    jmp .loop
.done:
    ret

parse_line:
    mov esi, line_buf
    call skip_spaces
    mov [cmd_ptr], esi
    mov al, [esi]
    test al, al
    jnz .scan
    mov [arg_ptr], esi
    ret
.scan:
    mov al, [esi]
    test al, al
    jz .end
    cmp al, ' '
    je .split
    inc esi
    jmp .scan
.split:
    mov byte [esi], 0
    inc esi
    call skip_spaces
    mov [arg_ptr], esi
    ret
.end:
    mov [arg_ptr], esi
    ret

arg_to_token:
    mov esi, [arg_ptr]
    call skip_spaces
    mov edi, token_buf
    mov ecx, NAME_MAX - 1
.copy:
    mov al, [esi]
    test al, al
    jz .done
    cmp al, ' '
    je .done
    mov [edi], al
    inc edi
    inc esi
    dec ecx
    jnz .copy
.done:
    mov byte [edi], 0
    ; trim trailing '/' characters while preserving "/" root.
    mov ebx, edi
.trim:
    cmp ebx, token_buf
    jbe .ret
    dec ebx
    cmp byte [ebx], '/'
    jne .ret
    cmp ebx, token_buf
    je .ret
    mov byte [ebx], 0
    jmp .trim
.ret:
    ret

lower_pair:
    cmp al, 'A'
    jb .l1
    cmp al, 'Z'
    ja .l1
    add al, 32
.l1:
    cmp bl, 'A'
    jb .done
    cmp bl, 'Z'
    ja .done
    add bl, 32
.done:
    ret

cmd_equals:
    ; in: ESI=pattern, compares [cmd_ptr]
    mov edi, [cmd_ptr]
.loop:
    mov al, [esi]
    mov bl, [edi]
    call lower_pair
    cmp al, bl
    jne .no
    test al, al
    jz .yes
    inc esi
    inc edi
    jmp .loop
.yes:
    mov eax, 1
    ret
.no:
    xor eax, eax
    ret

str_equals_ci:
    ; in: ESI, EDI
.loop:
    mov al, [esi]
    mov bl, [edi]
    call lower_pair
    cmp al, bl
    jne .no
    test al, al
    jz .yes
    inc esi
    inc edi
    jmp .loop
.yes:
    mov eax, 1
    ret
.no:
    xor eax, eax
    ret

; ------------------------------------------------------------
; CWD and simple root dynamic entries
; ------------------------------------------------------------

init_state:
    mov byte [cwd_mode], CWD_ROOT
    mov byte [cwd_custom_idx], 0xFF
    xor eax, eax
    mov edi, custom_used
    mov ecx, CUSTOM_MAX
    rep stosb
    ret

resolve_target_mode:
    ; in: ESI path token
    ; out: AL mode, AH custom idx (if mode=CWD_CUSTOM), CF=0 success, CF=1 fail
    mov ah, 0xFF
    mov al, [esi]
    test al, al
    jnz .non_empty
    mov al, [cwd_mode]
    mov ah, [cwd_custom_idx]
    clc
    ret
.non_empty:
    cmp al, '/'
    jne .no_slash
    inc esi
.no_slash:
    mov al, [esi]
    test al, al
    jnz .not_root
    mov al, CWD_ROOT
    mov ah, 0xFF
    clc
    ret
.not_root:
    ; split on first '/' so "xdv-core/src" resolves to "xdv-core".
    mov edi, esi
.seg_scan:
    mov al, [edi]
    test al, al
    jz .seg_ready
    cmp al, '/'
    je .seg_split
    inc edi
    jmp .seg_scan
.seg_split:
    mov byte [edi], 0
.seg_ready:
    ; dot and dotdot
    cmp byte [esi], '.'
    jne .name
    cmp byte [esi + 1], 0
    jne .dotdot
    mov al, [cwd_mode]
    mov ah, [cwd_custom_idx]
    clc
    ret
.dotdot:
    cmp byte [esi + 1], '.'
    jne .name
    cmp byte [esi + 2], 0
    jne .name
    mov al, CWD_ROOT
    mov ah, 0xFF
    clc
    ret

.name:
    mov edi, name_xdv_core
    call str_equals_ci
    test eax, eax
    jz .n1
    mov al, CWD_CORE
    mov ah, 0xFF
    clc
    ret
.n1:
    mov edi, name_xdv_utils
    call str_equals_ci
    test eax, eax
    jz .n2
    mov al, CWD_UTILS
    mov ah, 0xFF
    clc
    ret
.n2:
    mov edi, name_xdv_shell
    call str_equals_ci
    test eax, eax
    jz .n3
    mov al, CWD_SHELL
    mov ah, 0xFF
    clc
    ret
.n3:
    mov edi, name_xdv_edx
    call str_equals_ci
    test eax, eax
    jz .custom
    mov al, CWD_EDX
    mov ah, 0xFF
    clc
    ret

.custom:
    call find_custom_entry
    cmp al, 0xFF
    jne .has_custom
    stc
    ret
.has_custom:
    mov bl, al
    mov al, [custom_type + ebx]
    cmp al, ENTRY_DIR
    jne .fail
    mov al, CWD_CUSTOM
    mov ah, bl
    clc
    ret
.fail:
    stc
    ret

find_custom_entry:
    ; in: ESI name
    ; out: AL idx or 0xFF
    xor ebx, ebx
.scan:
    cmp ebx, CUSTOM_MAX
    jae .fail
    cmp byte [custom_used + ebx], 1
    jne .next
    mov edi, custom_names
    imul eax, ebx, NAME_MAX
    add edi, eax
    push esi
    call str_equals_ci
    pop esi
    test eax, eax
    jnz .hit
.next:
    inc ebx
    jmp .scan
.hit:
    mov al, bl
    ret
.fail:
    mov al, 0xFF
    ret

create_custom_entry:
    ; in: ESI name token, BL type
    ; out: AL idx or 0xFF
    call find_custom_entry
    cmp al, 0xFF
    je .find_slot
    mov al, 0xFE
    ret
.find_slot:
    xor ebx, ebx
.scan:
    cmp ebx, CUSTOM_MAX
    jae .full
    cmp byte [custom_used + ebx], 0
    je .slot
    inc ebx
    jmp .scan
.slot:
    mov byte [custom_used + ebx], 1
    mov al, [create_type_tmp]
    mov byte [custom_type + ebx], al
    mov edi, custom_names
    imul eax, ebx, NAME_MAX
    add edi, eax
    mov ecx, NAME_MAX
    xor eax, eax
    rep stosb
    mov edi, custom_names
    imul eax, ebx, NAME_MAX
    add edi, eax
    mov ecx, NAME_MAX - 1
    mov esi, [create_name_ptr]
.copy:
    mov al, [esi]
    mov [edi], al
    test al, al
    jz .done
    inc esi
    inc edi
    dec ecx
    jnz .copy
.done:
    mov byte [edi], 0
    mov al, bl
    ret
.full:
    mov al, 0xFF
    ret

delete_custom_entry:
    ; in: AL idx
    movzx ebx, al
    mov byte [custom_used + ebx], 0
    ret

; ------------------------------------------------------------
; Commands
; ------------------------------------------------------------

dispatch:
    mov esi, [cmd_ptr]
    mov al, [esi]
    test al, al
    jz .ret

    mov esi, cmd_help
    call cmd_equals
    test eax, eax
    jnz do_help
    mov esi, cmd_ls
    call cmd_equals
    test eax, eax
    jnz do_ls
    mov esi, cmd_cd
    call cmd_equals
    test eax, eax
    jnz do_cd
    mov esi, cmd_pwd
    call cmd_equals
    test eax, eax
    jnz do_pwd
    mov esi, cmd_cat
    call cmd_equals
    test eax, eax
    jnz do_cat
    mov esi, cmd_mkdir
    call cmd_equals
    test eax, eax
    jnz do_mkdir
    mov esi, cmd_rm
    call cmd_equals
    test eax, eax
    jnz do_rm
    mov esi, cmd_echo
    call cmd_equals
    test eax, eax
    jnz do_echo
    mov esi, cmd_ps
    call cmd_equals
    test eax, eax
    jnz do_ps
    mov esi, cmd_exit
    call cmd_equals
    test eax, eax
    jnz do_exit
    mov esi, cmd_edx
    call cmd_equals
    test eax, eax
    jnz do_edx

    ; xdv-core
    mov esi, cmd_console
    call cmd_equals
    test eax, eax
    jnz core_console
    mov esi, cmd_init
    call cmd_equals
    test eax, eax
    jnz core_init
    mov esi, cmd_io
    call cmd_equals
    test eax, eax
    jnz core_io
    mov esi, cmd_memory
    call cmd_equals
    test eax, eax
    jnz core_memory
    mov esi, cmd_process
    call cmd_equals
    test eax, eax
    jnz core_process
    mov esi, cmd_scheduler
    call cmd_equals
    test eax, eax
    jnz core_scheduler
    mov esi, cmd_strings
    call cmd_equals
    test eax, eax
    jnz core_strings
    mov esi, cmd_runtime_admin
    call cmd_equals
    test eax, eax
    jnz core_runtime_admin
    mov esi, cmd_runtime_admin2
    call cmd_equals
    test eax, eax
    jnz core_runtime_admin
    mov esi, cmd_sysmon
    call cmd_equals
    test eax, eax
    jnz core_sysmon
    mov esi, cmd_service
    call cmd_equals
    test eax, eax
    jnz core_service
    mov esi, cmd_log
    call cmd_equals
    test eax, eax
    jnz core_log
    mov esi, cmd_storage
    call cmd_equals
    test eax, eax
    jnz core_storage
    mov esi, cmd_security
    call cmd_equals
    test eax, eax
    jnz core_security
    mov esi, cmd_recovery
    call cmd_equals
    test eax, eax
    jnz core_recovery
    mov esi, cmd_cli
    call cmd_equals
    test eax, eax
    jnz core_cli

    ; xdv-xdvfs-utils
    mov esi, cmd_probe
    call cmd_equals
    test eax, eax
    jnz utils_probe
    mov esi, cmd_partition
    call cmd_equals
    test eax, eax
    jnz utils_partition
    mov esi, cmd_mkfs
    call cmd_equals
    test eax, eax
    jnz utils_mkfs
    mov esi, cmd_fsck
    call cmd_equals
    test eax, eax
    jnz utils_fsck
    mov esi, cmd_dir
    call cmd_equals
    test eax, eax
    jnz do_ls
    mov esi, cmd_file
    call cmd_equals
    test eax, eax
    jnz utils_file
    mov esi, cmd_mount
    call cmd_equals
    test eax, eax
    jnz utils_mount
    mov esi, cmd_space
    call cmd_equals
    test eax, eax
    jnz utils_space
    mov esi, cmd_perm
    call cmd_equals
    test eax, eax
    jnz utils_perm

    mov esi, msg_not_found
    call print_line
    ret
.ret:
    ret

do_help:
    mov esi, msg_help_shell
    call print_line
    mov esi, msg_help_core
    call print_line
    mov esi, msg_help_utils
    call print_line
    ret

do_pwd:
    mov al, [cwd_mode]
    cmp al, CWD_ROOT
    je .root
    cmp al, CWD_CORE
    je .core
    cmp al, CWD_UTILS
    je .utils
    cmp al, CWD_SHELL
    je .shell
    cmp al, CWD_EDX
    je .edx
    ; custom
    mov esi, path_root
    call puts
    mov al, '/'
    call putc
    movzx ebx, byte [cwd_custom_idx]
    mov edi, custom_names
    imul eax, ebx, NAME_MAX
    add edi, eax
    mov esi, edi
    call print_line
    ret
.root:
    mov esi, path_root
    call print_line
    ret
.core:
    mov esi, path_core
    call print_line
    ret
.utils:
    mov esi, path_utils
    call print_line
    ret
.shell:
    mov esi, path_shell
    call print_line
    ret
.edx:
    mov esi, path_edx
    call print_line
    ret

do_cd:
    call arg_to_token
    mov esi, token_buf
    call resolve_target_mode
    jc .nf
    mov [cwd_mode], al
    mov [cwd_custom_idx], ah
    mov esi, msg_ok
    call print_line
    ret
.nf:
    mov esi, msg_not_found
    call print_line
    ret

do_ls:
    call arg_to_token
    mov esi, token_buf
    mov al, [esi]
    test al, al
    jz .use_cwd
    call resolve_target_mode
    jc .nf
    mov [tmp_mode], al
    mov [tmp_custom_idx], ah
    jmp .list
.use_cwd:
    mov al, [cwd_mode]
    mov ah, [cwd_custom_idx]
    mov [tmp_mode], al
    mov [tmp_custom_idx], ah
.list:
    mov al, [tmp_mode]
    cmp al, CWD_ROOT
    je .root
    cmp al, CWD_CORE
    je .core
    cmp al, CWD_UTILS
    je .utils
    cmp al, CWD_SHELL
    je .shell
    cmp al, CWD_EDX
    je .edx
    mov esi, msg_empty
    call print_line
    ret

.root:
    mov esi, name_xdv_core
    call puts
    mov al, '/'
    call putc
    call newline
    mov esi, name_xdv_utils
    call puts
    mov al, '/'
    call putc
    call newline
    mov esi, name_xdv_shell
    call puts
    mov al, '/'
    call putc
    call newline
    mov esi, name_xdv_edx
    call puts
    mov al, '/'
    call putc
    call newline
    xor ebx, ebx
.custom_scan:
    cmp ebx, CUSTOM_MAX
    jae .done
    cmp byte [custom_used + ebx], 1
    jne .next
    mov edi, custom_names
    imul eax, ebx, NAME_MAX
    add edi, eax
    mov esi, edi
    call puts
    cmp byte [custom_type + ebx], ENTRY_DIR
    jne .nl
    mov al, '/'
    call putc
.nl:
    call newline
.next:
    inc ebx
    jmp .custom_scan
.done:
    ret
.nf:
    mov esi, msg_not_found
    call print_line
    ret

.core:
    mov esi, msg_core_cmds
    call print_space_list
    ret
.utils:
    mov esi, msg_utils_cmds
    call print_space_list
    ret
.shell:
    mov esi, name_readme
    call print_line
    mov esi, name_commands
    call print_line
    ret
.edx:
    mov esi, name_readme
    call print_line
    ret

do_cat:
    call arg_to_token
    mov esi, token_buf
    mov al, [esi]
    test al, al
    jnz .has_arg
    mov esi, msg_usage_cat
    call print_line
    ret
.has_arg:
    ; absolute paths for built-in preload files.
    cmp byte [esi], '/'
    jne .rel
    mov edi, path_shell_readme
    call str_equals_ci
    test eax, eax
    jz .abs_shell_cmds
    mov esi, msg_shell_readme
    call print_line
    ret
.abs_shell_cmds:
    mov esi, token_buf
    mov edi, path_shell_commands
    call str_equals_ci
    test eax, eax
    jz .abs_edx_readme
    mov esi, msg_shell_commands
    call print_line
    ret
.abs_edx_readme:
    mov esi, token_buf
    mov edi, path_edx_readme
    call str_equals_ci
    test eax, eax
    jz .abs_custom_root
    mov esi, msg_edx_readme
    call print_line
    ret
.abs_custom_root:
    ; support cat /<root-file> for dynamic files.
    mov esi, token_buf
    inc esi
    mov al, [esi]
    test al, al
    jz .nf
.abs_scan:
    mov al, [esi]
    test al, al
    jz .abs_lookup
    cmp al, '/'
    je .nf
    inc esi
    jmp .abs_scan
.abs_lookup:
    mov esi, token_buf
    inc esi
    call find_custom_entry
    cmp al, 0xFF
    jne .cf
    jmp .nf

.rel:
    mov al, [cwd_mode]
    cmp al, CWD_SHELL
    jne .not_shell
    mov edi, name_readme
    call str_equals_ci
    test eax, eax
    jz .shell_cmds
    mov esi, msg_shell_readme
    call print_line
    ret
.shell_cmds:
    mov esi, token_buf
    mov edi, name_commands
    call str_equals_ci
    test eax, eax
    jz .nf
    mov esi, msg_shell_commands
    call print_line
    ret

.not_shell:
    mov al, [cwd_mode]
    cmp al, CWD_EDX
    jne .root_custom
    mov edi, name_readme
    call str_equals_ci
    test eax, eax
    jz .nf
    mov esi, msg_edx_readme
    call print_line
    ret

.root_custom:
    mov al, [cwd_mode]
    cmp al, CWD_ROOT
    jne .nf
    mov esi, token_buf
    call find_custom_entry
    cmp al, 0xFF
    jne .cf
    jmp .nf
.cf:
    movzx ebx, al
    cmp byte [custom_type + ebx], ENTRY_FILE
    jne .not_file
    mov esi, msg_empty_file
    call print_line
    ret

.not_file:
    mov esi, msg_not_file
    call print_line
    ret
.nf:
    mov esi, msg_not_found
    call print_line
    ret

do_mkdir:
    mov al, [cwd_mode]
    cmp al, CWD_ROOT
    je .root
    mov esi, msg_root_only
    call print_line
    ret
.root:
    call arg_to_token
    mov esi, token_buf
    mov al, [esi]
    test al, al
    jnz .mk
    mov esi, msg_usage_mkdir
    call print_line
    ret
.mk:
    mov [create_name_ptr], esi
    mov byte [create_type_tmp], ENTRY_DIR
    mov bl, ENTRY_DIR
    call create_custom_entry
    cmp al, 0xFF
    je .full
    cmp al, 0xFE
    je .exists
    mov esi, msg_ok
    call print_line
    ret
.exists:
    mov esi, msg_exists
    call print_line
    ret
.full:
    mov esi, msg_table_full
    call print_line
    ret

do_rm:
    mov al, [cwd_mode]
    cmp al, CWD_ROOT
    je .root
    mov esi, msg_root_only
    call print_line
    ret
.root:
    call arg_to_token
    mov esi, token_buf
    mov al, [esi]
    test al, al
    jnz .go
    mov esi, msg_usage_rm
    call print_line
    ret
.go:
    call find_custom_entry
    cmp al, 0xFF
    jne .hit
    mov esi, msg_not_found
    call print_line
    ret
.hit:
    call delete_custom_entry
    mov esi, msg_ok
    call print_line
    ret

do_echo:
    mov esi, [arg_ptr]
    mov al, [esi]
    test al, al
    jnz .print
    call newline
    ret
.print:
    call print_line
    ret

do_ps:
    mov esi, msg_ps
    call print_line
    ret

do_exit:
    mov esi, msg_exit
    call print_line
    ret

do_edx:
    mov al, [cwd_mode]
    cmp al, CWD_ROOT
    je .root
    mov esi, msg_edx_scope
    call print_line
    ret
.root:
    call arg_to_token
    mov esi, token_buf
    mov al, [esi]
    test al, al
    jnz .open
    mov esi, msg_usage_edx
    call print_line
    ret
.open:
    call find_custom_entry
    cmp al, 0xFF
    jne .show
    mov [create_name_ptr], esi
    mov byte [create_type_tmp], ENTRY_FILE
    mov bl, ENTRY_FILE
    call create_custom_entry
.show:
    mov esi, msg_edx_open
    call puts
    mov esi, token_buf
    call print_line
    ret

; xdv-core
core_console:       mov esi, msg_core_console       ; fallthrough print
    jmp print_line
core_init:          mov esi, msg_core_init
    jmp print_line
core_io:            mov esi, msg_core_io
    jmp print_line
core_memory:        mov esi, msg_core_memory
    jmp print_line
core_process:       mov esi, msg_core_process
    jmp print_line
core_scheduler:     mov esi, msg_core_scheduler
    jmp print_line
core_strings:       mov esi, msg_core_strings
    jmp print_line
core_runtime_admin: mov esi, msg_core_runtime_admin
    jmp print_line
core_sysmon:        mov esi, msg_core_sysmon
    jmp print_line
core_service:       mov esi, msg_core_service
    jmp print_line
core_log:           mov esi, msg_core_log
    jmp print_line
core_storage:       mov esi, msg_core_storage
    jmp print_line
core_security:      mov esi, msg_core_security
    jmp print_line
core_recovery:      mov esi, msg_core_recovery
    jmp print_line
core_cli:           mov esi, msg_core_cli
    jmp print_line

; xdv-xdvfs-utils
utils_probe:        mov esi, msg_utils_probe
    jmp print_line
utils_partition:    mov esi, msg_utils_partition
    jmp print_line
utils_mkfs:         mov esi, msg_utils_mkfs
    jmp print_line
utils_fsck:         mov esi, msg_utils_fsck
    jmp print_line
utils_file:
    mov al, [cwd_mode]
    cmp al, CWD_ROOT
    je .root
    mov esi, msg_root_only
    call print_line
    ret
.root:
    call arg_to_token
    mov esi, token_buf
    mov al, [esi]
    test al, al
    jnz .mk
    mov esi, msg_usage_file
    call print_line
    ret
.mk:
    mov [create_name_ptr], esi
    mov byte [create_type_tmp], ENTRY_FILE
    mov bl, ENTRY_FILE
    call create_custom_entry
    cmp al, 0xFF
    je .full
    cmp al, 0xFE
    je .exists
    mov esi, msg_ok
    call print_line
    ret
.exists:
    mov esi, msg_exists
    call print_line
    ret
.full:
    mov esi, msg_table_full
    call print_line
    ret

utils_mount:        mov esi, msg_utils_mount
    jmp print_line
utils_space:        mov esi, msg_utils_space
    jmp print_line
utils_perm:         mov esi, msg_utils_perm
    jmp print_line

; ------------------------------------------------------------
; Data
; ------------------------------------------------------------

cursor           dd VGA_BASE
line_len         dd 0
line_buf         times LINE_MAX db 0
cmd_ptr          dd line_buf
arg_ptr          dd line_buf
token_buf        times NAME_MAX db 0
tmp_mode         db 0
tmp_custom_idx   db 0xFF

cwd_mode         db CWD_ROOT
cwd_custom_idx   db 0xFF

custom_used      times CUSTOM_MAX db 0
custom_type      times CUSTOM_MAX db 0
custom_names     times (CUSTOM_MAX * NAME_MAX) db 0
create_name_ptr  dd token_buf
create_type_tmp  db 0

kb_shift         db 0
kb_caps          db 0
kb_ctrl          db 0
kb_alt           db 0
kb_ext           db 0

map_unshift:
    db 0,27,'1','2','3','4','5','6','7','8','9','0','-','=',8,9
    db 'q','w','e','r','t','y','u','i','o','p','[',']',10,0,'a','s'
    db 'd','f','g','h','j','k','l',0x3B,0x27,0x60,0,0x5C,'z','x','c','v'
    db 'b','n','m',0x2C,0x2E,0x2F,0,'*',0,' ',0,0,0,0,0,0
    db 0,0,0,0,0,0,0,'7','8','9','-','4','5','6','+','1'
    db '2','3','0','.',0,0,0,0,0,0,0,0,0,0,0,0
    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

map_shift:
    db 0,27,'!','@','#','$','%','^','&','*','(',')','_','+',8,9
    db 'Q','W','E','R','T','Y','U','I','O','P','{','}',10,0,'A','S'
    db 'D','F','G','H','J','K','L',0x3A,0x22,0x7E,0,0x7C,'Z','X','C','V'
    db 'B','N','M',0x3C,0x3E,0x3F,0,'*',0,' ',0,0,0,0,0,0
    db 0,0,0,0,0,0,0,'7','8','9','-','4','5','6','+','1'
    db '2','3','0','.',0,0,0,0,0,0,0,0,0,0,0,0
    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

prompt           db '#:',0
path_root        db '/',0
path_core        db '/xdv-core',0
path_utils       db '/xdv-xdvfs-utils',0
path_shell       db '/xdv-shell',0
path_edx         db '/xdv-edx',0

cmd_help         db 'help',0
cmd_ls           db 'ls',0
cmd_cd           db 'cd',0
cmd_pwd          db 'pwd',0
cmd_cat          db 'cat',0
cmd_mkdir        db 'mkdir',0
cmd_rm           db 'rm',0
cmd_echo         db 'echo',0
cmd_ps           db 'ps',0
cmd_exit         db 'exit',0
cmd_edx          db 'edx',0

cmd_console      db 'console',0
cmd_init         db 'init',0
cmd_io           db 'io',0
cmd_memory       db 'memory',0
cmd_process      db 'process',0
cmd_scheduler    db 'scheduler',0
cmd_strings      db 'strings',0
cmd_runtime_admin db 'runtime-admin',0
cmd_runtime_admin2 db 'runtime_admin',0
cmd_sysmon       db 'sysmon',0
cmd_service      db 'service',0
cmd_log          db 'log',0
cmd_storage      db 'storage',0
cmd_security     db 'security',0
cmd_recovery     db 'recovery',0
cmd_cli          db 'cli',0

cmd_probe        db 'probe',0
cmd_partition    db 'partition',0
cmd_mkfs         db 'mkfs',0
cmd_fsck         db 'fsck',0
cmd_dir          db 'dir',0
cmd_file         db 'file',0
cmd_mount        db 'mount',0
cmd_space        db 'space',0
cmd_perm         db 'perm',0

name_xdv_core    db 'xdv-core',0
name_xdv_utils   db 'xdv-xdvfs-utils',0
name_xdv_shell   db 'xdv-shell',0
name_xdv_edx     db 'xdv-edx',0
name_readme      db 'README.txt',0
name_commands    db 'COMMANDS.txt',0

msg_ok           db 'ok',0
msg_not_found    db 'not found',0
msg_not_file     db 'not a file',0
msg_exists       db 'already exists',0
msg_empty        db '(empty)',0
msg_empty_file   db '(empty file)',0
msg_table_full   db 'table full',0
msg_root_only    db 'operation supported at / only in this build',0
msg_usage_cat    db 'usage: cat <name>',0
msg_usage_mkdir  db 'usage: mkdir <name>',0
msg_usage_rm     db 'usage: rm <name>',0
msg_usage_file   db 'usage: file <name>',0
msg_usage_edx    db 'usage: edx <name>',0
msg_ps           db 'pid 1 kernel_main running',0
msg_exit         db 'exit: shutdown not enabled in bare-metal mode',0
msg_edx_scope    db 'edx path support is root-local in this build',0
msg_edx_open     db 'edx: opened ',0

msg_help_shell   db 'shell: cd ls cat mkdir rm echo ps help exit edx pwd',0
msg_help_core    db 'xdv-core: console init io memory process scheduler strings runtime-admin sysmon service log storage security recovery cli',0
msg_help_utils   db 'xdv-xdvfs-utils: probe partition mkfs fsck dir file mount space perm',0

msg_shell_readme   db 'xdv-shell command layer',0
msg_shell_commands db 'commands: cd ls cat mkdir rm echo ps help exit edx pwd',0
msg_edx_readme     db 'edx editor module',0
msg_core_cmds      db 'console init io memory process scheduler strings runtime-admin sysmon service log storage security recovery cli',0
msg_utils_cmds     db 'probe partition mkfs fsck dir file mount space perm',0

msg_core_console       db 'xdv-core.console: operational',0
msg_core_init          db 'xdv-core.init: operational',0
msg_core_io            db 'xdv-core.io: operational',0
msg_core_memory        db 'xdv-core.memory: operational',0
msg_core_process       db 'xdv-core.process: operational',0
msg_core_scheduler     db 'xdv-core.scheduler: operational',0
msg_core_strings       db 'xdv-core.strings: operational',0
msg_core_runtime_admin db 'xdv-core.runtime-admin: operational',0
msg_core_sysmon        db 'xdv-core.sysmon: operational',0
msg_core_service       db 'xdv-core.service: operational',0
msg_core_log           db 'xdv-core.log: operational',0
msg_core_storage       db 'xdv-core.storage: operational',0
msg_core_security      db 'xdv-core.security: operational',0
msg_core_recovery      db 'xdv-core.recovery: operational',0
msg_core_cli           db 'xdv-core.cli: operational',0

msg_utils_probe       db 'xdvfs-utils.probe: operational',0
msg_utils_partition   db 'xdvfs-utils.partition: operational',0
msg_utils_mkfs        db 'xdvfs-utils.mkfs: operational',0
msg_utils_fsck        db 'xdvfs-utils.fsck: operational',0
msg_utils_mount       db 'xdvfs-utils.mount: operational',0
msg_utils_space       db 'xdvfs-utils.space: operational',0
msg_utils_perm        db 'xdvfs-utils.perm: operational',0

path_shell_readme     db '/xdv-shell/README.txt',0
path_shell_commands   db '/xdv-shell/COMMANDS.txt',0
path_edx_readme       db '/xdv-edx/README.txt',0
