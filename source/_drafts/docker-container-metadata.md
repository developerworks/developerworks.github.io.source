title: Docker 容器/镜像元数据
categories:
  - docker
tags:
  - docker
toc: false
date: 2014-09-05 03:16:25
---

```
[{
    "Args": [
        "-D"
    ],
    "Config": {
        "AttachStderr": false,
        "AttachStdin": false,
        "AttachStdout": false,
        "Cmd": [
            "/usr/sbin/sshd",
            "-D"
        ],
        "CpuShares": 0,
        "Cpuset": "",
        "Domainname": "",
        "Entrypoint": null,
        "Env": [                    # 环境变量数组
            "HOME=/",
            "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
        ],
        "ExposedPorts": {           # 暴露的端口
            "22/tcp": {}
        },
        "Hostname": "b8e4f1e181f8", # 主机名
        "Image": "ssh",             # 容器对应的镜像名称
        "Memory": 0,                # 内存大小
        "MemorySwap": 0,            # 交换区大小
        "NetworkDisabled": false,   # 是否禁用网络
        "OnBuild": null,
        "OpenStdin": false,
        "PortSpecs": null,
        "StdinOnce": false,
        "Tty": false,
        "User": "",                 # 用户?
        "Volumes": null,            # 映射卷
        "WorkingDir": ""            # 工作目录
    },
    # 创建时间
    "Created": "2014-09-04T16:37:54.261943263Z",
    "Driver": "aufs",
    "ExecDriver": "native-0.2",
    # 主机配置
    "HostConfig": {
        "Binds": null,
        "CapAdd": null,
        "CapDrop": null,
        "ContainerIDFile": "",
        "Devices": [],
        "Dns": null,
        "DnsSearch": null,
        "Links": null,
        "LxcConf": [],
        "NetworkMode": "bridge",
        "PortBindings": {},
        "Privileged": false,
        "PublishAllPorts": true,
        "RestartPolicy": {
            "MaximumRetryCount": 0,
            "Name": ""
        },
        "VolumesFrom": null
    },
    "HostnamePath": "/var/lib/docker/containers/b8e4f1e181f8e5cbb55ec2d78dde5cda12327fe742569c4a98ccfcab796975cf/hostname",
    "HostsPath": "/var/lib/docker/containers/b8e4f1e181f8e5cbb55ec2d78dde5cda12327fe742569c4a98ccfcab796975cf/hosts",
    "Id": "b8e4f1e181f8e5cbb55ec2d78dde5cda12327fe742569c4a98ccfcab796975cf",
    "Image": "748b01b9abffb4a31c841618b8fb382db1b7603da8d4eaeaa114084b730ed47f",    # 镜像ID
    "MountLabel": "",               # 挂载标签
    "Name": "/duanzhongwei",        # 镜像名称
    "NetworkSettings": {            # 网络设置
        "Bridge": "docker0",
        "Gateway": "172.17.42.1",   # 网关
        "IPAddress": "172.17.0.5",  # IP地址
        "IPPrefixLen": 16,
        "PortMapping": null,        # 端口映射
        "Ports": {                  # 端口
            "22/tcp": [
                {
                    "HostIp": "0.0.0.0",
                    "HostPort": "49156"
                }
            ]
        }
    },
    "Path": "/usr/sbin/sshd",
    "ProcessLabel": "",
    "ResolvConfPath": "/var/lib/docker/containers/b8e4f1e181f8e5cbb55ec2d78dde5cda12327fe742569c4a98ccfcab796975cf/resolv.conf",
    "State": {
        "ExitCode": 0,
        "FinishedAt": "0001-01-01T00:00:00Z",
        "Paused": false,
        "Pid": 1979,
        "Restarting": false,
        "Running": true,
        "StartedAt": "2014-09-04T16:37:54.382532935Z"
    },
    "Volumes": {},
    "VolumesRW": {}
}
]
```
