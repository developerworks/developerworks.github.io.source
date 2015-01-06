title: Erlang HiPE选项
categories:
  - Erlang
tags:
  - HiPE
toc: false
date: 2015-01-05 21:49:11
---

- 在编译期间输出内部调试信息
    ```
    4> hipe:help_option(debug).
    debug - Outputs internal debugging information during compilation
    ```
- 自动加载产生的原生代码到内存中
    ```
    5> hipe:help_option(load).
    load - Automatically load the produced native code into memory
    ```
- Displays assembly listing with addresses and bytecode
    ```
    6> hipe:help_option(pp_asm).
    pp_asm - Displays assembly listing with addresses and bytecode
    Currently available for x86 only
    ```
- 显示输入BEAM代码
    ```
    7> hipe:help_option(pp_beam).
    pp_beam - Display the input BEAM code
    ```
- 显示中间HiPE-ICode代码
    ```
    8> hipe:help_option(pp_icode).
    pp_icode - Display the intermediate HiPE-ICode
    ```
- 显示生成的(后端相关)原生代码
    ```
    9> hipe:help_option(pp_native).
    pp_native - Display the generated (back-end specific) native code
    ```
- 显示中间HiPE-RTL代码
    ```
    10> hipe:help_option(pp_rtl).
    pp_rtl - Display the intermediate HiPE-RTL code
    ```
- 报告编译器不同阶段的编译时间
    ```
    12> hipe:help_option(time).
    time - Reports the compilation times for the different stages
    of the compiler.
        {time, Module}       reports timings for the module Module.
        特定模块
        {time, [M1, M2, M3]} reports timings for the specified modules.
        指定模块列表
        {time, all}          reports timings all modules.
        所有模块
        time                 reports timings for the main module.
        主模块
    ```
- 指定编译时间限制,单位毫秒, 必须为非负整数, 或原子'infinity', 当前默认限制为15分钟(900000毫秒)
    ```
    13> hipe:help_option(timeout).
    timeout - Specify compilation time limit in ms. Used as {timeout, LIMIT}.
        The limit must be a non-negative integer or the atom 'infinity'.
        The current default limit is 15 minutes (900000 ms).
    ```
- 输出完成了什么
    ```
    14> hipe:help_option(verbose).
    verbose - Output information about what is being done
    ```