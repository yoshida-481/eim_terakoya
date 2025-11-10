DECLARE
	
	IS_NOT_MULTIPLE	constant number := 0;
	IS_MULTIPLE 	constant number := 1;
	objTypeName varchar2(200) := '';
	objTypeOtherName varchar2(200) := '';
	objTypeId_MailNotification	number := 0;
	rscode number := 0;
	attTypeName varchar2(200) := '';
	attTypeOtherName varchar2(200) := '';
	attTypeId_Reply number := 0;
	attTypeId_NotificationOfTiming number := 0;
	attTypeId_ApprovalReqTiming number := 0;
	attTypeId_NotificationOfDest number := 0;
	objTypeId_ObjectType number;
	objName		varchar2(255);
	r_objId		number;
BEGIN
	
	select id into objTypeId_ObjectType from EIMOBJTYPE where name = 'オブジェクトタイプ';
	
	/*----------------------------------------------------
	--メール通知オブジェクトタイプと関連する属性タイプを作成して適用する
	/*----------------------------------------------------*/
	objTypeName := 'メール通知';
	objTypeOtherName := 'Mail notification';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_MailNotification, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_MailNotification, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_MailNotification, 'EN', objTypeOtherName, rscode);
	objName := to_char(objTypeId_MailNotification);
	--メール通知オブジェクトタイプのオブジェクトを作成する
	ObjectUtils.createObject(1, null, objTypeId_ObjectType, objName, 0, r_objId, rscode);
	
	select id into attTypeId_Reply from EIMATTR where name = '受信確認';
	
	attTypeName := '公開通知タイミング';
	attTypeOtherName := 'Notification of opening to the public timing';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_NotificationOfTiming, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NotificationOfTiming, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NotificationOfTiming, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認依頼通知タイミング';
	attTypeOtherName := 'Approval request notification timing';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_ApprovalReqTiming, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ApprovalReqTiming, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ApprovalReqTiming, 'EN', attTypeOtherName, rscode);
	attTypeName := '公開通知送信先';
	attTypeOtherName := 'Notification of opening to the public destination';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_MULTIPLE, attTypeId_NotificationOfDest, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NotificationOfDest, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NotificationOfDest, 'EN', attTypeOtherName, rscode);
	
	--属性を適用する
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotification, attTypeId_NotificationOfTiming, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotification, attTypeId_ApprovalReqTiming, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotification, attTypeId_Reply, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotification, attTypeId_NotificationOfDest, rscode);
	
	commit;
	
END;
/


