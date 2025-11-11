/***********************************************/
/*                                             */
/*   Create Document Database Resources Script */
/*                                             */
/***********************************************/

--set serveroutput on;

accept EIM_INDEX_TABLE_SPACE char default 'eimidx' prompt 'Enter EIMANAGER Index Table Space Name: '

declare
	IS_NOT_MULTIPLE	constant number := 0;
	IS_MULTIPLE 	constant number := 1;

	rscode				number;
	objId				number;
	objName				varchar2(255);
	objOtherName		varchar2(255);
	objTypeId			number;
	objTypeName			varchar2(255);
	objTypeOtherName	varchar2(255);
	relTypeName			varchar2(255);
	relTypeOtherName	varchar2(255);
	attTypeId			number;
	attTypeName			varchar2(255);
	attTypeOtherName	varchar2(255);
	secId				number;
	secName				varchar(255);
	secOtherName		varchar(255);
	entryId				number;
	priority			number;

	objTypeId_AttrValueMaster		number;
	objTypeId_AttrTree				number;
	objTypeId_AttrDisclosureWord	number;
	objTypeId_AttrPositionAttr		number;
	objTypeId_WorkflowSetting		number;
	objTypeId_WorkflowExhibition	number;
	objTypeId_Document				number;
	objTypeId_WorkSpace				number;
	objTypeId_Folder				number;
	
	
	objTypeId_pdf					number;

	objTypeId_PublicEntry			number;
	objTypeId_PDFMerge				number;
	objTypeId_PDFSplit				number;
	objTypeId_PDFSignature			number;
	objTypeId_PDFSecurity			number;
	objTypeId_SignAndEncrypt		number;
	objTypeId_Tag					number;
	objTypeId_ValueDisplayColor		number;
	objTypeId_DocumentTypeSecurity	number;
	objTypeId_SystemSecurity		number;

	objTypeId_DocComapare			number;
	objTypeId_TempStore				number;

	objTypeId_MailNotification		number;

	relTypeId_Link					number;
	relTypeId_Branch				number;

	attTypeId_ListOfAttrNum			number;
	attTypeId_ListOfAttrDate		number;
	attTypeId_ListOfAttrStr			number;
	attTypeId_ListOfAttrText		number;
	attTypeId_ListOfAttrColor		number;
	attTypeId_ListOfAttrColorSet	number;
	attTypeId_AttrTreeObject		number;
	attTypeId_AttrTreeWordID		number;
	attTypeId_AttrTreeWordName		number;
	attTypeId_AttrTreePosAttrID		number;
	attTypeId_AttrTreePos			number;
	attTypeId_IndicationFlag		number;
	attTypeId_DefaultSettingFlag	number;
	attTypeId_EmailNoticeMethod		number;
	attTypeId_EmailNoticeFlag		number;
	attTypeId_WaitingPopupFlag		number;
	attTypeId_PDFConversionFlag		number;
	attTypeId_DateSettingPrdNum		number;
	attTypeId_DateSettingPrdUnit	number;
	attTypeId_ListOfAttrDouble		number;

	attTypeId_Path					number;
	attTypeId_Property				number;
	attTypeId_RevUpComment			number;
	attTypeId_CreateUserId			number;
	attTypeId_Expire				number;
	attTypeId_Comment				number;
	attTypeId_Reply					number;

	attTypeId_ModifyUserId			number;
	attTypeId_ModifyDate			number;
	attTypeId_CreateDate			number;
	attTypeId_FileSize				number;
	attTypeId_PreviousSecurity		number;
	attTypeId_AttrFromHighRank		number;
	attTypeId_HigherWFFolder		number;
	attTypeId_PublicProcFailure		number;
	attTypeId_NameAllotmentAttr		number;
	attTypeId_AttrToLowRank			number;
	attTypeId_LowerFolderMngSec		number;

	attTypeId_DefaultNotifiedFlag	number;
	attTypeId_PDFSplitFlag			number;
	attTypeId_PDFSignatureFlag		number;
	attTypeId_EntryTypeId			number;
	attTypeId_EntryTargetId			number;
	attTypeId_DocumentTypeId		number;
	attTypeId_ParentObjectId		number;
	attTypeId_MergeTargetObjectId	number;
	attTypeId_RegistUser			number;
	attTypeId_SignatureFlag			number;
	attTypeId_InsertApproveDate		number;
	attTypeId_InsertPage			number;
	attTypeId_InsertPlace			number;
	attTypeId_InsertPlaceX			number;
	attTypeId_InsertPlaceY			number;
	attTypeId_SetSecurityFlag		number;
	attTypeId_SetSecurityPWFlag		number;
	attTypeId_SecurityPassword		number;
	attTypeId_SetReadPWFlag			number;
	attTypeId_ReadPassword			number;
	attTypeId_AcceptPrintFlag		number;
	attTypeId_AcceptEditFlag		number;
	attTypeId_AcceptAddNoteFlag		number;
	attTypeId_AcceptReprintFlag		number;

	attTypeId_FailedPDFMerge		number;
	attTypeId_PDFSplitStatus		number;
	attTypeId_TargetToLink			number;
	attTypeId_DeleteBranchFlag		number;
	attTypeId_PDFSignatureStatus	number;
	attTypeId_InsertApproverFlag	number;
	attTypeId_DocumentLink			number;

	attTypeId_TargetDocument		number;
	attTypeId_Operator				number;
	attTypeId_SelectedTag			number;
	attTypeId_Tag					number;
	attTypeId_TagGiver				number;
	attTypeId_TagGivenDate			number;
	attTypeId_SignEncryptStatus		number;
	attTypeId_SignEncryptVer		number;
	attTypeId_DisplayColor			number;

	attTypeId_SourceCompDoc			number;
	attTypeId_DestCompDoc			number;
	attTypeId_NotifMailFlag			number;
	attTypeId_AnalyzeLayoutFlag		number;
	attTypeId_ZIPFileName			number;
	attTypeId_ZipRegistoryPath		number;
	
	attTypeId_NotificationOfTiming	number;
	attTypeId_ApprovalReqTiming		number;
	attTypeId_NotificationOfDest	number;
	
begin

	/* =================== */
	/* Create Object Types */
	/* Create Object Type Security */
	/* =================== */

	objTypeName := 'オブジェクトタイプ';
	select id into objTypeId_DocumentTypeSecurity from EIMOBJTYPE where name = objTypeName;
	objTypeName := 'ドキュメント';
	select id into objTypeId_Document from EIMOBJTYPE where name = objTypeName;
	objTypeName := 'ワークスペース';
	select id into objTypeId_WorkSpace from EIMOBJTYPE where name = objTypeName;
	objTypeName := 'フォルダ';
	select id into objTypeId_Folder from EIMOBJTYPE where name = objTypeName;

	objTypeName := 'systemセキュリティ';
	objTypeOtherName := 'SYSTEM Security';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_SystemSecurity, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_SystemSecurity, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_SystemSecurity, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_SystemSecurity);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '属性タイプ値マスター';
	objTypeOtherName := 'Attribute type value master';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_AttrValueMaster, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_AttrValueMaster, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_AttrValueMaster, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_AttrValueMaster);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '属性ツリー';
	objTypeOtherName := 'Attribute tree';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_AttrTree, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_AttrTree, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_AttrTree, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_AttrTree);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '属性ツリー他言語';
	objTypeOtherName := 'Attribute tree disclosure word';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_AttrDisclosureWord, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_AttrDisclosureWord, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_AttrDisclosureWord, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_AttrDisclosureWord);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '属性ツリー所属属性';
	objTypeOtherName := 'Attribute tree position attribute';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_AttrPositionAttr, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_AttrPositionAttr, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_AttrPositionAttr, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_AttrPositionAttr);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := 'ワークフロー設定';
	objTypeOtherName := 'Workflow setting';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_WorkflowSetting, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_WorkflowSetting, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_WorkflowSetting, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_WorkflowSetting);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := 'ワークフロー公開処理';
	objTypeOtherName := 'Workflow exhibition processing';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_WorkflowExhibition, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_WorkflowExhibition, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_WorkflowExhibition, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_WorkflowExhibition);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '公開通知先エントリー';
	objTypeOtherName := 'Public entry';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_PublicEntry, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PublicEntry, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PublicEntry, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_PublicEntry);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := 'PDF結合';
	objTypeOtherName := 'PDF merge';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_PDFMerge, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PDFMerge, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PDFMerge, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_PDFMerge);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := 'PDF分割';
	objTypeOtherName := 'PDF split';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_PDFSplit, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PDFSplit, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PDFSplit, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_PDFSplit);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := 'PDF署名';
	objTypeOtherName := 'PDF signature';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_PDFSignature, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PDFSignature, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PDFSignature, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_PDFSignature);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := 'PDFセキュリティ設定';
	objTypeOtherName := 'PDF security setting';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_PDFSecurity, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PDFSecurity, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_PDFSecurity, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_PDFSecurity);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '署名・暗号化';
	objTypeOtherName := 'Signature and encryption';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_SignAndEncrypt, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_SignAndEncrypt, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_SignAndEncrypt, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_SignAndEncrypt);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := 'タグ';
	objTypeOtherName := 'Tag';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_Tag, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_Tag, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_Tag, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_Tag);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '属性表示色';
	objTypeOtherName := 'Value display color';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_ValueDisplayColor, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_ValueDisplayColor, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_ValueDisplayColor, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_ValueDisplayColor);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '公開ファイル比較';
	objTypeOtherName := 'Public document comparison';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_DocComapare, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_DocComapare, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_DocComapare, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_DocComapare);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	objTypeName := '一時格納ファイル';
	objTypeOtherName := 'Temporarily stored file';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_TempStore, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_TempStore, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_TempStore, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_TempStore);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
	
/* c00で実施するので不要
	objTypeName := 'メール通知';
	objTypeOtherName := 'Mail notification';
	ObjectUtils.createObjectType(null, null, objTypeName, null, objTypeId_MailNotification, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_MailNotification, 'JA', objTypeName, rscode);
	ObjectUtils.addOtherObjectTypeName(objTypeId_MailNotification, 'EN', objTypeOtherName, rscode);
	objName := TO_CHAR(objTypeId_MailNotification);
	ObjectUtils.createObject(1, null, objTypeId_DocumentTypeSecurity, objName, 0, objId, rscode);
*/

	/* ===================== */
	/* Create Relation Types */
	/* ===================== */
	relTypeName := 'リンク';
	relTypeOtherName := 'Link';
	RelationUtils.createRelationType(null, null, relTypeName, 3, 0, relTypeId_Link, rscode);
	RelationUtils.addOtherRelationTypeName(relTypeId_Link, 'JA', relTypeName, rscode);
	RelationUtils.addOtherRelationTypeName(relTypeId_Link, 'EN', relTypeOtherName, rscode);
	relTypeName := 'ブランチ';
	relTypeOtherName := 'Branch';
	RelationUtils.createRelationType(null, null, relTypeName, 3, 0, relTypeId_Branch, rscode);
	RelationUtils.addOtherRelationTypeName(relTypeId_Branch, 'JA', relTypeName, rscode);
	RelationUtils.addOtherRelationTypeName(relTypeId_Branch, 'EN', relTypeOtherName, rscode);

	/* ====================== */
	/* Create Attribute Types */
	/* ====================== */
	attTypeName := 'パス';
	select id into attTypeId_Path from EIMATTR where name = attTypeName;
	attTypeName := 'プロパティ';
	select id into attTypeId_Property from EIMATTR where name = attTypeName;
	attTypeName := '作成者';
	select id into attTypeId_CreateUserId from EIMATTR where name = attTypeName;
	attTypeName := '有効期限';
	select id into attTypeId_Expire from EIMATTR where name = attTypeName;
	attTypeName := '受信確認';
	select id into attTypeId_Reply from EIMATTR where name = attTypeName;
	
	attTypeName := '属性タイプ値マスター数値リスト';
	attTypeOtherName := 'List of attribute type value master numerical value';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_ListOfAttrNum, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrNum, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrNum, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性タイプ値マスター日付値リスト';
	attTypeOtherName := 'List of attribute type value master date values';
	AttributeUtils.createAttributeType(null, null, attTypeName, 3, IS_MULTIPLE, attTypeId_ListOfAttrDate, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrDate, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrDate, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性タイプ値マスター文字列値リスト';
	attTypeOtherName := 'List of attribute type value master character string values';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_MULTIPLE, attTypeId_ListOfAttrStr, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrStr, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrStr, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性タイプ値マスターテキスト値リスト';
	attTypeOtherName := 'List of attribute type value master text values';
	AttributeUtils.createAttributeType(null, null, attTypeName, 4, IS_MULTIPLE, attTypeId_ListOfAttrText, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrText, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrText, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性タイプ値マスターダブル数字リスト';
	attTypeOtherName := 'List of attribute type value master double values';
	AttributeUtils.createAttributeType(null, null, attTypeName, 5, IS_MULTIPLE, attTypeId_ListOfAttrDouble, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrDouble, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrDouble, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性タイプ値マスター表示色リスト';
	attTypeOtherName := 'List of attribute type value master display color values';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_MULTIPLE, attTypeId_ListOfAttrColor, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrColor, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrColor, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性タイプ値マスター表示設定リスト';
	attTypeOtherName := 'List of attribute type value master display setting values';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_ListOfAttrColorSet, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrColorSet, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ListOfAttrColorSet, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性ツリー分類対象';
	attTypeOtherName := 'Attribute tree classification object';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_AttrTreeObject, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreeObject, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreeObject, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性ツリー他言語ID';
	attTypeOtherName := 'Attribute tree disclosure word ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_AttrTreeWordID, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreeWordID, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreeWordID, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性ツリー他言語名称';
	attTypeOtherName := 'Attribute tree disclosure word name';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_AttrTreeWordName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreeWordName, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreeWordName, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性ツリー所属属性ID';
	attTypeOtherName := 'Attribute tree position attribute ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_AttrTreePosAttrID, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreePosAttrID, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreePosAttrID, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性ツリー所属属性ポジション';
	attTypeOtherName := 'Attribute tree position attribute position';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_AttrTreePos, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreePos, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrTreePos, 'EN', attTypeOtherName, rscode);
	attTypeName := '属性ツリー所属属性値なし表示フラグ';
	attTypeOtherName := 'Indication flag which there is no value in';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_IndicationFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_IndicationFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_IndicationFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー設定承認依頼先デフォルト設定フラグ';
	attTypeOtherName := 'Workflow setting approval request point default setting flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_DefaultSettingFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DefaultSettingFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DefaultSettingFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー設定メール通知方法のデフォルト設定';
	attTypeOtherName := 'Default setting of the workflow setting email notice method';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_EmailNoticeMethod, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_EmailNoticeMethod, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_EmailNoticeMethod, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー設定差戻し・取戻しメール通知フラグ';
	attTypeOtherName := 'Workflow setting referring a case back to the original court / a recovering email notice flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_EmailNoticeFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_EmailNoticeFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_EmailNoticeFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー設定処理待ちポップアップ通知フラグ';
	attTypeOtherName := 'Notice of workflow setting processing waiting popup flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_WaitingPopupFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_WaitingPopupFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_WaitingPopupFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー公開処理PDF変換実施フラグ';
	attTypeOtherName := 'Workflow exhibition processing PDF conversion enforcement flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_PDFConversionFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFConversionFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFConversionFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー公開処理有効期限設定期間数字';
	attTypeOtherName := 'Workflow exhibition processing expiration date setting period number';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_DateSettingPrdNum, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DateSettingPrdNum, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DateSettingPrdNum, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー公開処理有効期限設定期間単位';
	attTypeOtherName := 'Workflow exhibition processing expiration date setting period unit';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_DateSettingPrdUnit, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DateSettingPrdUnit, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DateSettingPrdUnit, 'EN', attTypeOtherName, rscode);
	attTypeName := '前セキュリティ';
	attTypeOtherName := 'Previous security';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_PreviousSecurity, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PreviousSecurity, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PreviousSecurity, 'EN', attTypeOtherName, rscode);
	attTypeName := '上位からの引継ぎ属性';
	attTypeOtherName := 'Transfer attribute from the high rank';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_AttrFromHighRank, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrFromHighRank, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrFromHighRank, 'EN', attTypeOtherName, rscode);
	attTypeName := '上位WFフォルダ';
	attTypeOtherName := 'Higher WF folder';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_HigherWFFolder, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_HigherWFFolder, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_HigherWFFolder, 'EN', attTypeOtherName, rscode);
	attTypeName := '公開処理失敗';
	attTypeOtherName := 'Public processing failure';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_PublicProcFailure, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicProcFailure, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PublicProcFailure, 'EN', attTypeOtherName, rscode);
	attTypeName := '名称割当て属性';
	attTypeOtherName := 'Name allotment attribute';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_NameAllotmentAttr, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NameAllotmentAttr, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NameAllotmentAttr, 'EN', attTypeOtherName, rscode);
	attTypeName := '下位への引継ぎ属性';
	attTypeOtherName := 'Transfer attribute to the low rank';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_AttrToLowRank, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrToLowRank, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AttrToLowRank, 'EN', attTypeOtherName, rscode);
	attTypeName := '下位フォルダ管理セキュリティ';
	attTypeOtherName := 'Lower folder management security';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_LowerFolderMngSec, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LowerFolderMngSec, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_LowerFolderMngSec, 'EN', attTypeOtherName, rscode);

	attTypeName := 'ワークフロー設定公開通知先デフォルト設定フラグ';
	attTypeOtherName := 'Workflow setting default notified setting flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_DefaultNotifiedFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DefaultNotifiedFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DefaultNotifiedFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー公開処理PDF分割実施フラグ';
	attTypeOtherName := 'Workflow exhibition processing PDF split enforcement flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_PDFSplitFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFSplitFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFSplitFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ワークフロー公開処理PDF署名実施フラグ';
	attTypeOtherName := 'Workflow exhibition processing PDF signature enforcement flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_PDFSignatureFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFSignatureFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFSignatureFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'エントリータイプID';
	attTypeOtherName := 'Entry type ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_EntryTypeId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_EntryTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_EntryTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := 'エントリー対象ID';
	attTypeOtherName := 'Entry targer ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_EntryTargetId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_EntryTargetId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_EntryTargetId, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ドキュメントタイプID';
	attTypeOtherName := 'Document type ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_DocumentTypeId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DocumentTypeId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DocumentTypeId, 'EN', attTypeOtherName, rscode);
	attTypeName := '親オブジェクトID';
	attTypeOtherName := 'Parent object ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_ParentObjectId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ParentObjectId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ParentObjectId, 'EN', attTypeOtherName, rscode);
	attTypeName := '結合対象オブジェクトID';
	attTypeOtherName := 'Merge target object ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_MergeTargetObjectId, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_MergeTargetObjectId, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_MergeTargetObjectId, 'EN', attTypeOtherName, rscode);
	attTypeName := '登録者';
	attTypeOtherName := 'Regist user';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_RegistUser, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_RegistUser, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_RegistUser, 'EN', attTypeOtherName, rscode);
	attTypeName := 'PDF署名ステータス';
	attTypeOtherName := 'PDF Signature status';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_PDFSignatureStatus, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFSignatureStatus, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFSignatureStatus, 'EN', attTypeOtherName, rscode);
	attTypeName := '署名有無';
	attTypeOtherName := 'Signature flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_SignatureFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SignatureFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SignatureFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認日付挿入';
	attTypeOtherName := 'Insert approve date flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_InsertApproveDate, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertApproveDate, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertApproveDate, 'EN', attTypeOtherName, rscode);
	attTypeName := '承認者名挿入';
	attTypeOtherName := 'Insert approver flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_InsertApproverFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertApproverFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertApproverFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := '挿入ページ';
	attTypeOtherName := 'Insert page';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_InsertPage, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertPage, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertPage, 'EN', attTypeOtherName, rscode);
	attTypeName := '挿入位置基準点';
	attTypeOtherName := 'Insert place';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_InsertPlace, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertPlace, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertPlace, 'EN', attTypeOtherName, rscode);
	attTypeName := '挿入位置座標X';
	attTypeOtherName := 'Insert place X';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_InsertPlaceX, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertPlaceX, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertPlaceX, 'EN', attTypeOtherName, rscode);
	attTypeName := '挿入位置座標Y';
	attTypeOtherName := 'Insert place Y';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_InsertPlaceY, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertPlaceY, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_InsertPlaceY, 'EN', attTypeOtherName, rscode);
	attTypeName := 'セキュリティ設定有無';
	attTypeOtherName := 'Setting security flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_SetSecurityFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SetSecurityFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SetSecurityFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'セキュリティパスワード設定有無';
	attTypeOtherName := 'Setting security password flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_SetSecurityPWFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SetSecurityPWFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SetSecurityPWFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'セキュリティパスワード';
	attTypeOtherName := 'Security Password';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_SecurityPassword, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SecurityPassword, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SecurityPassword, 'EN', attTypeOtherName, rscode);
	attTypeName := '参照用パスワード設定有無';
	attTypeOtherName := 'Setting read password flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_SetReadPWFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SetReadPWFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SetReadPWFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := '参照用パスワード';
	attTypeOtherName := 'Read Password';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_ReadPassword, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ReadPassword, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ReadPassword, 'EN', attTypeOtherName, rscode);
	attTypeName := '印刷許可設定';
	attTypeOtherName := 'Accept to print flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_AcceptPrintFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AcceptPrintFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AcceptPrintFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := '編集許可設定';
	attTypeOtherName := 'Accept to edit flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_AcceptEditFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AcceptEditFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AcceptEditFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := '注釈追加許可設定';
	attTypeOtherName := 'Accept to add note flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_AcceptAddNoteFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AcceptAddNoteFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AcceptAddNoteFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := '転載許可設定';
	attTypeOtherName := 'Accept to reprint flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_AcceptReprintFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AcceptReprintFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AcceptReprintFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'PDF結合処理失敗';
	attTypeOtherName := 'Failed to PDF merge';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_FailedPDFMerge, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_FailedPDFMerge, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_FailedPDFMerge, 'EN', attTypeOtherName, rscode);
	attTypeName := 'PDF分割状態';
	attTypeOtherName := 'PDF split status';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_PDFSplitStatus, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFSplitStatus, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFSplitStatus, 'EN', attTypeOtherName, rscode);
	attTypeName := 'リンク先';
	attTypeOtherName := 'Target to link';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_TargetToLink, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TargetToLink, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TargetToLink, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ドキュメントリンク';
	attTypeOtherName := 'Document link';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_DocumentLink, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DocumentLink, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DocumentLink, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ブランチ先削除';
	attTypeOtherName := 'Delete branch';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_DeleteBranchFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DeleteBranchFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DeleteBranchFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := '対象ドキュメント';
	attTypeOtherName := 'Target document';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_TargetDocument, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TargetDocument, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TargetDocument, 'EN', attTypeOtherName, rscode);
	attTypeName := '実施者';
	attTypeOtherName := 'Operator';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_Operator, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Operator, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Operator, 'EN', attTypeOtherName, rscode);
	attTypeName := '選択タグ';
	attTypeOtherName := 'Selected tag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_SelectedTag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectedTag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SelectedTag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'タグ';
	attTypeOtherName := 'Tag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_Tag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Tag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_Tag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'タグ付与者';
	attTypeOtherName := 'Tag giver';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_TagGiver, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TagGiver, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TagGiver, 'EN', attTypeOtherName, rscode);
	attTypeName := 'タグ付与日';
	attTypeOtherName := 'Tag given date';
	AttributeUtils.createAttributeType(null, null, attTypeName, 3, IS_MULTIPLE, attTypeId_TagGivenDate, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TagGivenDate, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_TagGivenDate, 'EN', attTypeOtherName, rscode);
	attTypeName := '署名・暗号化状態';
	attTypeOtherName := 'Signature and encryption status';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_SignEncryptStatus, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SignEncryptStatus, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SignEncryptStatus, 'EN', attTypeOtherName, rscode);
	attTypeName := '署名・暗号化バージョン';
	attTypeOtherName := 'Signature and encryption version';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_SignEncryptVer, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SignEncryptVer, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SignEncryptVer, 'EN', attTypeOtherName, rscode);
	attTypeName := '表示色';
	attTypeOtherName := 'Display color';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_DisplayColor, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DisplayColor, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DisplayColor, 'EN', attTypeOtherName, rscode);

	attTypeName := '比較元ドキュメントオブジェクトID';
	attTypeOtherName := 'Source comparison document';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_SourceCompDoc, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SourceCompDoc, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SourceCompDoc, 'EN', attTypeOtherName, rscode);
	attTypeName := '比較対象ドキュメントオブジェクトID';
	attTypeOtherName := 'Destination comparison document';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_DestCompDoc, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DestCompDoc, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_DestCompDoc, 'EN', attTypeOtherName, rscode);
	attTypeName := '完了通知メール設定';
	attTypeOtherName := 'Notification mail flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_NotifMailFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NotifMailFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_NotifMailFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'レイアウト解析設定';
	attTypeOtherName := 'Analyze layout flag';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_AnalyzeLayoutFlag, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AnalyzeLayoutFlag, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_AnalyzeLayoutFlag, 'EN', attTypeOtherName, rscode);
	attTypeName := 'ZIPファイル名';
	attTypeOtherName := 'ZIP name';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_ZIPFileName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ZIPFileName, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ZIPFileName, 'EN', attTypeOtherName, rscode);
	attTypeName := '登録先パス';
	attTypeOtherName := 'Registory Path';
	AttributeUtils.createAttributeType(null, null, attTypeName, 2, IS_NOT_MULTIPLE, attTypeId_ZipRegistoryPath, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ZipRegistoryPath, 'JA', attTypeName, rscode);
	AttributeUtils.addOtherAttributeTypeName(attTypeId_ZipRegistoryPath, 'EN', attTypeOtherName, rscode);

/* c00で実施するので不要
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
*/

	/* ============================================= */
	/* Apply Object Type To AttributeTypeValueMaster */
	/* ============================================= */
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrValueMaster, attTypeId_ListOfAttrNum, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrValueMaster, attTypeId_ListOfAttrDate, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrValueMaster, attTypeId_ListOfAttrStr, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrValueMaster, attTypeId_ListOfAttrText, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrValueMaster, attTypeId_ListOfAttrDouble, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrValueMaster, attTypeId_ListOfAttrColor, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrValueMaster, attTypeId_ListOfAttrColorSet, rscode);

	/* ================================== */
	/* Apply Object Type To AttributeTree */
	/* ================================== */
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrTree, attTypeId_AttrTreeObject, rscode);

	/* ================================================ */
	/* Apply Object Type To AttributeTreeDisclosureWord */
	/* ================================================ */
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrDisclosureWord, attTypeId_AttrTreeWordID, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrDisclosureWord, attTypeId_AttrTreeWordName, rscode);

	/* =================================================== */
	/* Apply Object Type To AttributeTreePositionAttribute */
	/* =================================================== */
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrPositionAttr, attTypeId_AttrTreePosAttrID, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrPositionAttr, attTypeId_AttrTreePos, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_AttrPositionAttr, attTypeId_IndicationFlag, rscode);

	/* ==================================== */
	/* Apply Object Type To WorkflowSetting */
	/* ==================================== */
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowSetting, attTypeId_DefaultSettingFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowSetting, attTypeId_DefaultNotifiedFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowSetting, attTypeId_EmailNoticeMethod, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowSetting, attTypeId_EmailNoticeFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowSetting, attTypeId_WaitingPopupFlag, rscode);

	/* ================================================= */
	/* Apply Object Type To WorkflowExhibitionProcessing */
	/* ================================================= */
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_PDFConversionFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_DateSettingPrdNum, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_DateSettingPrdUnit, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_PDFSplitFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_PDFSignatureFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_SignatureFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_InsertApproveDate, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_InsertApproverFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_InsertPage, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_InsertPlace, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_InsertPlaceX, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_InsertPlaceY, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_SetSecurityFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_SetSecurityPWFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_SecurityPassword, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_SetReadPWFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_ReadPassword, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_AcceptPrintFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_AcceptEditFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_AcceptAddNoteFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkflowExhibition, attTypeId_AcceptReprintFlag, rscode);

	/* ============================ */
	/* Apply Object Type To Document*/
	/* ============================ */
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_PreviousSecurity, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_AttrFromHighRank, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_HigherWFFolder, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_PublicProcFailure, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_FailedPDFMerge, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_PDFSplitStatus, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_TargetToLink, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_DocumentLink, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_Tag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_TagGiver, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_TagGivenDate, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_SignEncryptStatus, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_SignEncryptVer, rscode);

	/* ============================== */
	/* Apply Object Type To WorkSpace */
	/* ============================== */
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkSpace, attTypeId_NameAllotmentAttr, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkSpace, attTypeId_AttrToLowRank, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_WorkSpace, attTypeId_LowerFolderMngSec, rscode);

	/* =========================== */
	/* Apply Object Type To Folder */
	/* =========================== */
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_AttrFromHighRank, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_Expire, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_PreviousSecurity, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_HigherWFFolder, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_Tag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_TagGiver, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Folder, attTypeId_TagGivenDate, rscode);

	/* ================================ */
	/* Apply Object Type To PublicEntry */
	/* ================================ */
	ObjectAttributeUtils.applyAttributeType(objTypeId_PublicEntry, attTypeId_EntryTypeId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PublicEntry, attTypeId_EntryTargetId, rscode);

	/* ============================= */
	/* Apply Object Type To PDFMerge */
	/* ============================= */
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFMerge, attTypeId_DocumentTypeId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFMerge, attTypeId_ParentObjectId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFMerge, attTypeId_MergeTargetObjectId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFMerge, attTypeId_RegistUser, rscode);

	/* ================================= */
	/* Apply Object Type To PDFSignature */
	/* ================================= */
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_PDFSignatureStatus, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_SignatureFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_InsertApproveDate, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_InsertApproverFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_InsertPage, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_InsertPlace, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_InsertPlaceX, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_InsertPlaceY, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_SetSecurityFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_SetSecurityPWFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_SecurityPassword, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_SetReadPWFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_ReadPassword, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_AcceptPrintFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_AcceptEditFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_AcceptAddNoteFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSignature, attTypeId_AcceptReprintFlag, rscode);

	/* ================================ */
	/* Apply Object Type To PDFSecurity */
	/* ================================ */
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_RegistUser, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_SetSecurityFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_SetSecurityPWFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_SecurityPassword, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_SetReadPWFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_ReadPassword, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_AcceptPrintFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_AcceptEditFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_AcceptAddNoteFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_PDFSecurity, attTypeId_AcceptReprintFlag, rscode);

	/* ============================================= */
	/* Apply Object Type To Signature and Encryption */
	/* ============================================= */
	ObjectAttributeUtils.applyAttributeType(objTypeId_SignAndEncrypt, attTypeId_TargetDocument, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_SignAndEncrypt, attTypeId_Operator, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_SignAndEncrypt, attTypeId_SelectedTag, rscode);

	/* ======================== */
	/* Apply Object Type To Tag */
	/* ======================== */
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_Path, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_Property, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_CreateUserId, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_Expire, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_Tag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_TagGiver, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_TagGivenDate, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_Tag, attTypeId_SignEncryptStatus, rscode);

	/* ======================================== */
	/* Apply Object Type To Value Display Color */
	/* ======================================== */
	ObjectAttributeUtils.applyAttributeType(objTypeId_ValueDisplayColor, attTypeId_DisplayColor, rscode);

	/* =============================================== */
	/* Apply Object Type To Public document comparison */
	/* =============================================== */
	ObjectAttributeUtils.applyAttributeType(objTypeId_DocComapare, attTypeId_SourceCompDoc, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_DocComapare, attTypeId_DestCompDoc, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_DocComapare, attTypeId_NotifMailFlag, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_DocComapare, attTypeId_AnalyzeLayoutFlag, rscode);

	/* ============================================= */
	/* Apply Object Type To Temporarily stored file */
	/* ============================================= */
	ObjectAttributeUtils.applyAttributeType(objTypeId_TempStore, attTypeId_ZIPFileName, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_TempStore, attTypeId_ZipRegistoryPath, rscode);

	/* ============================================= */
	/* Apply Object Type To Mail notification */
	/* ============================================= */
/* c00で実施するので不要
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotification, attTypeId_NotificationOfTiming, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotification, attTypeId_ApprovalReqTiming, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotification, attTypeId_Reply, rscode);
	ObjectAttributeUtils.applyAttributeType(objTypeId_MailNotification, attTypeId_NotificationOfDest, rscode);
*/

	/* ============================= */
	/* Apply Relation Type To Branch */
	/* ============================= */
	RelationAttributeUtils.applyAttributeType(relTypeId_Branch, attTypeId_DeleteBranchFlag, rscode);

	/* ============================== */
	/* Insert Application Access Role */
	/* ============================== */
/* a03で実施するので不要
	insert into EIMACRTYPE values(500, 'ROLE_500');
	insert into EIMACRTYPEOTHER values(500, 'JA', '常時読取');
	insert into EIMACRTYPEOTHER values(500, 'EN', 'Always read');
*/

	/* ====================== */
	/* Create System Security */
	/* ====================== */

/* 初期登録時に追加されているので不要
	secName := 'system';
	SecurityUtils.createSecurity(secName, secId, rscode);
	SecurityUtils.createAccessEntry(secId, 1, 1, entryId, priority, rscode);
	SecurityUtils.updateAccessRole(entryId, 11, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 12, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 13, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 14, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 15, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 21, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 22, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 31, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 32, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 41, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 42, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 51, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 61, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 62, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 63, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 101, 0, 1, rscode);
	SecurityUtils.updateAccessRole(entryId, 500, 0, 1, rscode);
*/
	/* 言語＝日本語 */
--	insert into EIMSECOTHER values(secId, 'JA', 'system');
	/* 言語＝英語 */
--	insert into EIMSECOTHER values(secId, 'EN', 'system');

	/* ======================== */
	/* Update System User Admin */
	/* ======================== */
	--c05で実施するので不要
	--update EIMUser set ADMIN = '1023' where ID = '1';
	
	/*---------------------------------------------------------*/
	--パス属性タイプを複数値にする
	/*---------------------------------------------------------*/
	update EIMATTR set is_multiple = 1 where name = 'パス';
	
	/*---------------------------------------------------------*/
	--パス属性値を複数値対応にするためにEIMOBJSTRの
	--KEY値を0から1に更新する
	/*---------------------------------------------------------*/
	update EIMOBJSTR set key = 1
	where
	key = 0
	and type = (select id from EIMATTR where name = 'パス');
	
	commit;
	
end;
/

--以下、デバッグのためにコメントアウト。本番は必ず外すこと。
/***************************************/
/* Document Management For Oracle Text */
/***************************************/
create table EIMPAGE
(
	id		number(32)		not null,
	fmt		varchar2(16)	not null,
	cset	varchar2(16)	not null,
	path	varchar2(512)	not null,
	page	number(32)		not null
);

/* For Oracle Text */
exec ctx_ddl.create_preference('EIMPAGE_FILTER',   'INSO_FILTER');
exec ctx_ddl.create_preference('EIMPAGE_LEXER',   'JAPANESE_VGRAM_LEXER');
exec ctx_ddl.create_preference('EIMPAGE_DATASTORE', 'FILE_DATASTORE');
exec ctx_ddl.create_preference('EIMPAGE_STORAGE','BASIC_STORAGE');
exec ctx_ddl.set_attribute('EIMPAGE_STORAGE', 'I_TABLE_CLAUSE', 'tablespace ' || '&EIM_INDEX_TABLE_SPACE' );
exec ctx_ddl.set_attribute('EIMPAGE_STORAGE', 'K_TABLE_CLAUSE', 'tablespace ' || '&EIM_INDEX_TABLE_SPACE' );
exec ctx_ddl.set_attribute('EIMPAGE_STORAGE', 'R_TABLE_CLAUSE', 'tablespace ' || '&EIM_INDEX_TABLE_SPACE' );
exec ctx_ddl.set_attribute('EIMPAGE_STORAGE', 'N_TABLE_CLAUSE', 'tablespace ' || '&EIM_INDEX_TABLE_SPACE' );
exec ctx_ddl.set_attribute('EIMPAGE_STORAGE', 'I_INDEX_CLAUSE', 'tablespace ' || '&EIM_INDEX_TABLE_SPACE' );

create index EIMPAGEIDX
on EIMPAGE(path)
indextype is ctxsys.context
parameters('STORAGE EIMPAGE_STORAGE  FILTER EIMPAGE_FILTER LEXER EIMPAGE_LEXER DATASTORE EIMPAGE_DATASTORE FORMAT COLUMN FMT CHARSET COLUMN CSET');


