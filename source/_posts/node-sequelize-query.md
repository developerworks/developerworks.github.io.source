title: Sequelize 查询
categories:
  - Node.js
tags:
  - sequelize
date: 2014-08-15
toc: false
---


`sequelize.query()`方法以及参数


```
sequelize.query('your query', [, callee], [, options], [, replacements])
```

简单查询

```
sequelize.query("SELECT * FROM users").success(function(users) {
  console.log(users)
})
```

映射到模型定义

```
sequelize
  .query('SELECT * FROM users', Users)
  .success(function(users){
    console.log(users)
  })
```

用户模型定义如下:

```
var moment = require('moment');
module.exports = function (sequelize, DataTypes) {
    var user = sequelize.define('user', {
        id: {
            type: DataTypes.INTEGER(11).UNSIGNED,
            primaryKey: true,
            autoIncrement: true,
            unique: true,
            allowNull: false,
            comment: '用户ID',
            description: '用户ID'
        },
        username: {
            type: DataTypes.STRING,
            allowNull: false,
            comment: '用户名',
            description: '用于登陆的用户名称',
            search: true
        },
        hashed_password: {
            type: DataTypes.STRING,
            allowNull: false,
            comment: '密码,可为明文,也可以为密码的SHA1值,如果是HASH值,长度必须等于40',
            description: '密码HASH',
            search: false
        },
        mail: {
            type: DataTypes.STRING,
            allowNull: true,
            comment: '邮件地址',
            defaultValue: '',
            search: true
        },
        admin: {
            type: DataTypes.BOOLEAN,
            allowNull: false,
            defaultValue: 0
        },
        status: {
            type: DataTypes.INTEGER(11).UNSIGNED,
            allowNull: false,
            defaultValue: 0,
            comment: '用户状态'
        },
        last_login: {
            type: DataTypes.DATE,
            allowNull: true,
            comment: '上次登录时间,时间格式为 YYYY-MM-DD HH:mm:ss',
            defaultValue: null,
            get: function(){
                return moment(this.getDataValue('updated_at')).format('YYYY-MM-DD HH:mm:ss');
            }
        },
        salt: {
            type: DataTypes.STRING,
            allowNull: true,
            defaultValue: null
        }
    }, {
        comment: '用户基本数据表',
        getterMethods: {
            created_at: function () {
                return moment(this.getDataValue('created_at')).format('YYYY-MM-DD HH:mm:ss');
            },
            updated_at: function () {
                return moment(this.getDataValue('updated_at')).format('YYYY-MM-DD HH:mm:ss');
            }
        },
        classMethods: {
            associate: function (models) {
                user.hasMany(models.device);
                user.hasMany(models.business);
                user.hasMany(models.billing);
                user.hasMany(models.favorite);
                user.belongsTo(models.payment_type);
                user.belongsTo(models.payment_method);
                user.hasOne(models.box);
            }
        }
    });
    return user;
};
```

选项是一个又如下keys的对象:

```
sequelize
  .query('SELECT 1', null, {
    // 一个函数(或为false) 用于记录每次发送到数据库服务器的SQL查询
    logging: console.log,
    // 如果plain设置为true,那么sequelize只返回结果集的第一行记录. 如果为false返回查询的所有记录
    plain: false,
    // 如果查询没有关联的模型定义,设置为true
    raw: false
  })
```

第二个参数为空,当我们设置`{raw:true}`,第二个参数将被忽略, 改查询任然返回一个原始对象

```
sequelize
  .query('SELECT * FROM projects', null, { raw: true })
  .success(function(projects) {
    console.log(projects)
  })
```