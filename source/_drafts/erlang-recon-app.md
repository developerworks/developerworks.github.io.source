title: Erlang Recon
categories:
  - Erlang
tags:
  - DevOps
toc: false
date: 2014-10-15 02:34:25
---

Recon is a library to be dropped into any other Erlang project, to be used to assist DevOps people diagnose problems in production nodes.

The source code can be obtained from [the github repo][1].

包含的模块:

1. [recon][2]
Main module, contains basic functionality to interact with the `recon` application. It includes functions to gather information about processes and the general state of the virtual machine, ports, and OTP behaviours running in the node. It also includes a few functions to facilitate RPC calls with distributed Erlang nodes.
2. [recon_alloc][3]
Regroups functions to deal with Erlang's memory allocators, or particularly, to try to present the allocator data in a way that makes it simpler to discover the presence of possible problems.
3. [recon_lib][4]
Regroups useful functionality used by `recon` when dealing with data from the node. Would be an interesting place to look if you were looking to extend Recon's functionality
4. [recon_trace][5]

Provides production-safe tracing facilities, to dig into the execution of programs and function calls as they are running.
This library contains few tests -- most of the functionality has been tried directly in production instead, and for many Erlang installs, Recon functionality should be safe to use directly in production, assuming there is still memory left to be used in the node.

To help with regular DevOps tasks, a variety of scripts has also been included in the repository's `script/` directory:

1. `app_deps.erl`
Escript that relies on graphviz, and produces a dependency graph of all applications in the repository.
The script can be run directly from an Erlang shell (if compiled), or as escript `app_deps.erl`.
2. `erl_crashdump_analyzer.sh`
Bash script to run on an Erlang crash dump as `./erl_crashdump_analyzer.sh <crashdump>` and will extract generic information that can be useful in determining the most common causes of node failure.
3. `queue_fun.awk`
Awk script to tun on an Erlang Crash dump as `awk -v threshold=<queue size> -f queue_fun.awk <crashdump>` and
will show what function processes with queue sizes larger or equal to `<queue size>` were operating at the time of the crash dump.
May help find out if most processes were stuck blocking on a given function call while accumulating messages forever.

  [1]: https://github.com/ferd/recon
  [2]: http://ferd.github.io/recon/recon.html
  [3]: http://ferd.github.io/recon/recon_alloc.html
  [4]: http://ferd.github.io/recon/recon_lib.html
  [5]: http://ferd.github.io/recon/recon_trace.html

## 参考资料

1. http://ferd.github.io/recon/