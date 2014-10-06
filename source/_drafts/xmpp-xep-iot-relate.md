title: XMPP 物联网相关协议扩展
categories:
  - XMPP
tags:
  - XMPP
  - XEP
  - IOT
toc: false
date: 2014-10-05 00:52:52
---


##  XMPP/IoT相关协议

- 高效数据互换格式
http://xmpp.org/extensions/xep-0322.html XEP-0322: Efficient XML Interchange (EXI) Format

- 物联网 - 传感器数据
http://xmpp.org/extensions/xep-0323.html XEP-0323: Internet of Things - Sensor Data
通过XMPP网络交换传感器数据,

- 物联网 - `Provisioning`
http://xmpp.org/extensions/xep-0324.html XEP-0324: Internet of Things - Provisioning

- 物联网 - 控制
http://xmpp.org/extensions/xep-0325.html XEP-0325: Internet of Things - Control

- 物联网 - 集中器
http://xmpp.org/extensions/xep-0326.html XEP-0326: Internet of Things - Concentrator

- 物联网 - 发现
http://xmpp.org/extensions/xep-0347.html XEP-0347: Internet of Things - Discovery

服务开通(英语：`Provisioning`)是一个电信行业的技术词汇,它是指准备和配备(preparing and equipping)一个网络,以允许其向它的用户提供(新)业务的处理过程.在(美国)国家安全/紧急准备电子通信(NS/EP telecommunications)业务中,服务开通等同于`开始`(initiation),并且包括改变一个已存在的优先服务或功能的状态.
英语`Provisioning`,的原意是`供应`,`预备`,电信行业中一般将其翻译成中文`服务开通`.也有文档称其为`开通`,`服务提供`,`服务供应`等.


##  OneM2M联盟规范文档

`oneM2M`是7个国家的电信联盟组成的合作机构,定制物联网机器到机器(M2M)应用层通信标准.

- 无线通讯解决方案联盟(ATIS)
- 中国通讯标准化协会(CCSA)
- 欧洲电信标准协会(ETSI)
- 韩国电信技术协会(TTA)
- 日本电信技术委员会(TTC)
- 美国电信工业协会(TIA)
- 及日本电波产业协会(ARIB)


http://www.onem2m.org/candidate_release/index.cfm

`M2M在线`
http://m2m.iot-online.com/


功能架构
TS-0001-oneM2M-Functional-Architecture-V-2014-08
需求
TS-0002-Requirements-V-2014-08
安全方案
TS-0003-Security_Solutions-V-2014-08
核心协议
TS-0004-CoreProtocol-V-2014-08
管理(OMA)
TS-0005-Management_Enablement (OMA)-V-2014-08
管理(BBF)
TS-0006-Management_Enablement_(BBF)-V-2014-08
CoAP协议绑定
TS-0008-CoAP_Protocol_Binding-V-2014-08
HTTP协议绑定
TS-0009-HTTP_Protocol_Binding_V-2014-08
定义和缩写词
TS-0011-Definitions and Acronyms-V-2014-08





## 资料

1. XMPP Service Discovery extensions for M2M and IoT
http://servicelab.org/2013/02/21/xmpp-service-discovery-extensions-for-m2m-and-iot/

2. oneM2M 候选规范
http://www.onem2m.org/candidate_release/index.cfm

3. http://m2m.iot-online.com/news/2013102224849.html


<!--
字段类型
    字段表示的值类型. 比如: 瞬时值(Momentary), 状态值(Status), 标识值(Identification), 计算值(Calculated), 峰值(Peak), 历史值(Historical),等.
Historical Value
    A value stored in memory from a previous timestamp.
Identification Value
    A value that can be used for identification. (Serial numbers, meter IDs, locations, names, etc.)
    Localization information
    Optional information for a field, allowing the sensor to control how the information should be presented to human viewers.
Meter
    A device possible containing multiple sensors, used in metering applications. Examples: Electricity meter, Water Meter, Heat Meter, Cooling Meter, etc.
Momentary Value
    A momentary value represents a value measured at the time of the read-out.
Node
    Graphs contain nodes and edges between nodes. In Internet of Things, sensors, actuators, meters, devices, gateways, etc., are often depicted as nodes whereas links between sensors (friendships) are depicted as edges. In abstract terms, it's easier to talk about a Node, rather than list different possible node types (sensors, actuators, meters, devices, gateways, etc.). Each Node has a Node ID.
Node ID
    An ID uniquely identifying a node within its corresponding context. If a globally unique ID is desired, an architecture should be used using a universally accepted ID scheme.
Parameter
    Readable and/or writable property on a node/device. The XEP-0326 Internet of Things - Concentrators (XEP-0326) [6] deals with reading and writing parameters on nodes/devices. Fields are not parameters, and parameters are not fields.
Peak Value
    A maximum or minimum value during a given period.
Precision
    In physics, precision determines the number of digits of precision. In sensor networks however, this definition is not easily applicable. Instead, precision determines, for example, the number of decimals of precision, or power of precision. Example: 123.200 MWh contains 3 decimals of precision. All entities parsing and delivering field information in sensor networks should always retain the number of decimals in a message.
Sensor
    Device measuring at least one digital value (0 or 1) or analog value (value with precision and physical unit). Examples: Temperature sensor, pressure sensor, etc. Sensor values are reported as fields during read-out. Each sensor has a unique Node ID.
SN
    Sensor Network. A network consisting, but not limited to sensors, where transport and use of sensor data is of primary concern. A sensor network may contain actuators, network applications, monitors, services, etc.
Status Value
    A value displaying status information about something.
Timestamp
    Timestamp of value, when the value was sampled or recorded.
Token
    A client, device or user can get a token from a provisioning server. These tokens can be included in requests to other entities in the network, so these entities can validate access rights with the provisioning server.
Unit
    Physical unit of value. Example: MWh, l/s, etc.
Value
    A field value.
Value Status
    Status of field value. Contains important status information for Quality of Service purposes. Examples: Ok, Error, Warning, Time Shifted, Missing, Signed, etc.
Value Type
    Can be numeric, string, boolean, Date & Time, Time Span or Enumeration.
WSN
    Wireless Sensor Network, a sensor network including wireless devices.
XMPP Client
    Application connected to an XMPP network, having a JID. Note that sensors, as well as applications requesting sensor data can be XMPP clients.
-->