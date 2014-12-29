title: Sequelize 定义索引
categories:
  - Node.js
tags:
  - Sequelize
toc: false
date: 2014-12-29 15:19:37
---

Node的ORM框架Sequelize 2.0版本支持在模型定义文件中定义索引的创建, 2.0之前的版本, 需要通过Migration的方式创建.

## V1.7版本的创建方式

```
migration.addIndex('partner', ['name', 'cellphone'])
```

## V2版本的创建方式

```js
var moment = require('moment');
module.exports = function (sequelize, DataTypes) {
  var partner = sequelize.define('partner', {
    id: {
      type: DataTypes.INTEGER,
      primaryKey: true,
      autoIncrement: true,
      comment: '合作伙伴ID',
      allowNull: false
    },
    name: {
      type: DataTypes.STRING(32),
      allowNull: false,
      comment: '合作伙伴名称,可以是个人也可以使组织机构',
      description: '合作伙伴名称,可以是个人也可以使组织机构'
    },
    concat: {
      type: DataTypes.STRING(16),
      allowNull: false,
      comment: '联系人姓名',
      description: '联系人姓名'

    },
    cellphone: {
      type: DataTypes.STRING(11),
      allowNull: false,
      comment: '联系人手机号码',
      description: '联系人手机号码'
    },
    email: {
      type: DataTypes.STRING(64),
      allowNull: true,
      comment: '联系人电子邮件地址',
      description: '联系人电子邮件地址'
    },
    status: {
      type: DataTypes.STRING(16),
      allowNull: false,
      defaultValue: 'PENDING', // 'APPROVED, REJECTED',
      comment: '合作伙伴状态,PENDING:等待审核,APPROVED:已经核准,REJECTED:已拒绝',
      description: '合作伙伴状态,PENDING:等待审核,APPROVED:已经核准,REJECTED:已拒绝'
    }
  }, {
    comment: '合作伙伴信息表',
    classMethods: {
      associate: function (models) {
        partner.hasMany(models.map);
      }
    },
    getterMethods: {
      created_at: function () {
        return moment(this.getDataValue('created_at')).format('YYYY-MM-DD');
      },
      updated_at: function () {
        return moment(this.getDataValue('updated_at')).format('YYYY-MM-DD HH:mm:ss');
      },
      deleted_at: function () {
        return moment(this.getDataValue('deleted_at')).format('YYYY-MM-DD HH:mm:ss');
      }
    },
    indexes: [
      {
        name: 'partner_unique_index_name',
        unique: true,
        method: 'BTREE',
        fields: ['name']
      },
      {
        name: 'partner_unique_index_cellphone',
        unique: true,
        method: 'BTREE',
        fields: ['cellphone']
      }
    ]
  });
  partner._status = {
    PENDING: 'PENDING',
    APPROVED: 'APPROVED',
    REJECTED: 'REJECTED'
  };
  return partner;
};
```