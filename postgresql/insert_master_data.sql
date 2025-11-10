/***********************************************/
/*                                             */
/*   Create Document Database Resources Script */
/*     for PostgreSQL                          */
/*                                             */
/***********************************************/

SET client_encoding = 'UTF8';
\set ON_ERROR_STOP ON

\prompt 'Enter Default Value Of UserBoxIntegrationFlag ( 0: not allow 1: allow ): ' default_UserBoxIntegrationFlag
\o /dev/null
select set_config('psql.default_UserBoxIntegrationFlag', :'default_UserBoxIntegrationFlag', false);
\o

DO $$
declare
	IS_NOT_MULTIPLE	constant		bigint := 0;
	IS_MULTIPLE 	constant		bigint := 1;

	rscode				bigint;
	objId				bigint;
	objName				text;
	objOtherName		text;
	objTypeId			bigint;
	objTypeName			text;
	objTypeOtherName	text;
	relTypeId			bigint;
	relTypeName			text;
	relTypeOtherName	text;
	attTypeId			bigint;
	attTypeName			text;
	attTypeOtherName	text;
	secId				bigint;
	secName				text;
	secOtherName		text;
	entryId				bigint;
	priority			bigint;
	objId_system		bigint;

	objTypeId_AttrValueMaster		bigint;
	objTypeId_AttrTree				bigint;
	objTypeId_AttrDisclosureWord	bigint;
	objTypeId_AttrPositionAttr		bigint;
	objTypeId_WorkflowSetting		bigint;
	objTypeId_WorkflowExhibition	bigint;
	objTypeId_Document				bigint;
	objTypeId_WorkSpace				bigint;
	objTypeId_Folder				bigint;
	objTypeId_Reply					bigint;
	objTypeId_Recycle				bigint;
	objTypeId_WorkspaceRecycle		bigint;
	objTypeId_pdf					bigint;
	objTypeId_favorite				bigint;
	objTypeId_mydoc					bigint;
	objTypeId_UserObjectType		bigint;

	objTypeId_PublicEntry			bigint;
	objTypeId_PDFMerge				bigint;
	objTypeId_PDFSplit				bigint;
	objTypeId_PDFSignature			bigint;
	objTypeId_PDFSecurity			bigint;
	objTypeId_SignAndEncrypt		bigint;
	objTypeId_Tag					bigint;
	objTypeId_ValueDisplayColor		bigint;
	objTypeId_DocumentTypeSecurity	bigint;
	objTypeId_SystemSecurity		bigint;

	objTypeId_DocComapare			bigint;
	objTypeId_TempStore				bigint;

	objTypeId_MailNotification		bigint;
	objTypeId_OCR_Processing		bigint;

	objTypeId_InsertURL				bigint;

	objTypeId_ObjectType			bigint;

	objTypeId_publicNotificationTemplate	bigint;

	relTypeId_Link					bigint;
	relTypeId_Branch				bigint;
	relTypeId_Recycle				bigint;

	attTypeId_ListOfAttrNum			bigint;
	attTypeId_ListOfAttrDate		bigint;
	attTypeId_ListOfAttrStr			bigint;
	attTypeId_ListOfAttrText		bigint;
	attTypeId_ListOfAttrColor		bigint;
	attTypeId_ListOfAttrColorSet	bigint;
	attTypeId_AttrTreeObject		bigint;
	attTypeId_AttrTreeWordID		bigint;
	attTypeId_AttrTreeWordName		bigint;
	attTypeId_AttrTreePosAttrID		bigint;
	attTypeId_AttrTreePos			bigint;
	attTypeId_IndicationFlag		bigint;
	attTypeId_DefaultSettingFlag	bigint;
	attTypeId_EmailNoticeMethod		bigint;
	attTypeId_EmailNoticeFlag		bigint;
	attTypeId_WaitingPopupFlag		bigint;
	attTypeId_BossApprovalFlag		bigint;
	attTypeId_BossOnlyDefaultFlag	bigint;
	attTypeId_PDFConversionFlag		bigint;
	attTypeId_DateSettingPrdNum		bigint;
	attTypeId_DateSettingPrdUnit	bigint;
	attTypeId_ListOfAttrDouble		bigint;
	attTypeId_PDFConversionExecDate	bigint;
	attTypeId_publicPDFPreRegistDate	bigint;

	attTypeId_Path					bigint;
	attTypeId_Property				bigint;
	attTypeId_RevUpComment			bigint;
	attTypeId_CreateUserId			bigint;
	attTypeId_Expire				bigint;
	attTypeId_Comment				bigint;
	attTypeId_Reply					bigint;

	attTypeId_ModifyUserId			bigint;
	attTypeId_ModifyDate			bigint;
	attTypeId_CreateDate			bigint;
	attTypeId_FileSize				bigint;
	attTypeId_PreviousSecurity		bigint;
	attTypeId_AttrFromHighRank		bigint;
	attTypeId_HigherWFFolder		bigint;
	attTypeId_PublicProcFailure		bigint;
	attTypeId_NameAllotmentAttr		bigint;
	attTypeId_AttrToLowRank			bigint;
	attTypeId_LowerFolderMngSec		bigint;

	attTypeId_DefaultNotifiedFlag	bigint;
	attTypeId_PDFSplitFlag			bigint;
	attTypeId_PDFSignatureFlag		bigint;
	attTypeId_EntryTypeId			bigint;
	attTypeId_EntryTargetId			bigint;
	attTypeId_DocumentTypeId		bigint;
	attTypeId_ParentObjectId		bigint;
	attTypeId_MergeTargetObjectId	bigint;
	attTypeId_RegistUser			bigint;
	attTypeId_SignatureFlag			bigint;
	attTypeId_InsertApproveDate		bigint;
	attTypeId_InsertPage			bigint;
	attTypeId_InsertPlace			bigint;
	attTypeId_InsertPlaceX			bigint;
	attTypeId_InsertPlaceY			bigint;
	attTypeId_SetSecurityFlag		bigint;
	attTypeId_SetSecurityPWFlag		bigint;
	attTypeId_SecurityPassword		bigint;
	attTypeId_SetReadPWFlag			bigint;
	attTypeId_ReadPassword			bigint;
	attTypeId_AcceptPrintFlag		bigint;
	attTypeId_AcceptEditFlag		bigint;
	attTypeId_AcceptAddNoteFlag		bigint;
	attTypeId_AcceptReprintFlag		bigint;

	attTypeId_FailedPDFMerge		bigint;
	attTypeId_PDFSplitStatus		bigint;
	attTypeId_TargetToLink			bigint;
	attTypeId_DeleteBranchFlag		bigint;
	attTypeId_PDFSignatureStatus	bigint;
	attTypeId_InsertApproverFlag	bigint;
	attTypeId_DocumentLink			bigint;

	attTypeId_TargetDocument		bigint;
	attTypeId_Operator				bigint;
	attTypeId_SelectedTag			bigint;
	attTypeId_Tag					bigint;
	attTypeId_TagGiver				bigint;
	attTypeId_TagGivenDate			bigint;
	attTypeId_SignEncryptStatus		bigint;
	attTypeId_SignEncryptVer		bigint;
	attTypeId_DisplayColor			bigint;

	attTypeId_SourceCompDoc			bigint;
	attTypeId_DestCompDoc			bigint;
	attTypeId_NotifMailFlag			bigint;
	attTypeId_AnalyzeLayoutFlag		bigint;
	attTypeId_ZIPFileName			bigint;
	attTypeId_ZipRegistoryPath		bigint;

	attTypeId_NotificationOfTiming	bigint;
	attTypeId_ApprovalReqTiming		bigint;
	attTypeId_NotificationOfDest	bigint;

	attTypeId_LimitDocTypeFlag			bigint;
	attTypeId_SelectableDocType			bigint;
	attTypeId_LimitFolderTypeFlag		bigint;
	attTypeId_SelectableFolderType		bigint;
	attTypeId_LimitTagTypeFlag			bigint;
	attTypeId_SelectableTagType			bigint;
	attTypeId_LimitSecurityFlag			bigint;
	attTypeId_SelectableSecurity		bigint;
	attTypeId_Administrator				bigint;
	attTypeId_AdministratorType			bigint;
	attTypeId_linkUpdateTiming			bigint;
	attTypeId_OCR_Status				bigint;
	attTypeId_OCR_Result_Status			bigint;
	attTypeId_OCR_Setting				bigint;
	attTypeId_ApproverCheckin			bigint;

	attTypeId_WebDAV_lock_flag			bigint;
	attTypeId_Number					bigint;
	attTypeId_RevisionUpTakeover		bigint;
	attTypeId_LatestRevAssociation		bigint;

	attTypeId_Select_Custom_Table		bigint;
	attTypeId_InsertURLFlag				bigint;
	attTypeId_SearchIndex				bigint;
	attTypeId_DocumentAttachment		bigint;
	attTypeId_TemporaryAttachment		bigint;
	attTypeId_PublicComment				bigint;
	attTypeId_PublicCommentLog			bigint;

	attTypeId_DeleteDate				bigint;

	attTypeId_SkipStatus				bigint;

	attTypeId_ApproveNameLang				bigint;
	attTypeId_SignJobName					bigint;

	attTypeId_PublicNotificationTemplateName	bigint;
	attTypeId_PublicNotificationTemplateUserId	bigint;
	attTypeId_PublicNotificationTemplateGroupId	bigint;
	attTypeId_PublicNotificationTemplateRoleId	bigint;
	attTypeId_PublicNotificationTemplateCompositeGroupId	bigint;
	
	attTypeId_ManualDeletionProhibitedFlag	bigint;
	attTypeId_UserBoxIntegrationFlag		bigint;
	default_UserBoxIntegrationFlag			bigint;
	
	attTypeId_ThumbnailConversionDisabledFlag	bigint;
	attTypeId_PreviewConversionDisabledFlag		bigint;

	resultId		bigint;
	resultTmpId		bigint;
	resultCursor	refcursor;
	errorValue		bigint;
	
begin

	/* =========================== */
	/* Create Object Types         */
	/* Create Object Type Security */
	/* =========================== */

	objTypeName := 'オブジェクトタイプ';
	select id into STRICT objTypeId_DocumentTypeSecurity from EIMOBJTYPE where name = objTypeName;

	objTypeName := 'systemセキュリティ';
	objTypeOtherName := 'SYSTEM Security';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_SystemSecurity, rscode, resultId);
	call othernameutils2.setothername(objTypeId_SystemSecurity, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_SystemSecurity, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_SystemSecurity;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '属性タイプ値マスター';
	objTypeOtherName := 'Attribute type value master';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_AttrValueMaster, rscode, resultId);
	call othernameutils2.setothername(objTypeId_AttrValueMaster, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_AttrValueMaster, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_AttrValueMaster;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '属性ツリー';
	objTypeOtherName := 'Attribute tree';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_AttrTree, rscode, resultId);
	call othernameutils2.setothername(objTypeId_AttrTree, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_AttrTree, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_AttrTree;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '属性ツリー他言語';
	objTypeOtherName := 'Attribute tree disclosure word';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_AttrDisclosureWord, rscode, resultId);
	call othernameutils2.setothername(objTypeId_AttrDisclosureWord, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_AttrDisclosureWord, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_AttrDisclosureWord;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '属性ツリー所属属性';
	objTypeOtherName := 'Attribute tree position attribute';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_AttrPositionAttr, rscode, resultId);
	call othernameutils2.setothername(objTypeId_AttrPositionAttr, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_AttrPositionAttr, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_AttrPositionAttr;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'ワークフロー設定';
	objTypeOtherName := 'Workflow setting';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_WorkflowSetting, rscode, resultId);
	call othernameutils2.setothername(objTypeId_WorkflowSetting, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_WorkflowSetting, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_WorkflowSetting;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'ワークフロー公開処理';
	objTypeOtherName := 'Workflow exhibition processing';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_WorkflowExhibition, rscode, resultId);
	call othernameutils2.setothername(objTypeId_WorkflowExhibition, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_WorkflowExhibition, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_WorkflowExhibition;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'ドキュメント';
	objTypeOtherName := 'Document';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_Document, rscode, resultId);
	call othernameutils2.setothername(objTypeId_Document, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_Document, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_Document;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'ワークスペース';
	objTypeOtherName := 'Workspace';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_WorkSpace, rscode, resultId);
	call othernameutils2.setothername(objTypeId_WorkSpace, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_WorkSpace, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_WorkSpace;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'フォルダ';
	objTypeOtherName := 'Folder';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_Folder, rscode, resultId);
	call othernameutils2.setothername(objTypeId_Folder, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_Folder, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_Folder;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '受信確認';
	objTypeOtherName := 'Receive confirmation';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_Reply, rscode, resultId);
	call othernameutils2.setothername(objTypeId_Reply, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_Reply, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_Reply;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'ごみ箱';
	objTypeOtherName := 'Garbage box';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_Recycle, rscode, resultId);
	call othernameutils2.setothername(objTypeId_Recycle, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_Recycle, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_Recycle;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'ワークスペース固有ごみ箱';
	objTypeOtherName := 'Workspace Garbage box';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_WorkspaceRecycle, rscode, resultId);
	call othernameutils2.setothername(objTypeId_WorkspaceRecycle, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_WorkspaceRecycle, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);

	objTypeName := 'PDF変換';
	objTypeOtherName := 'PDF conversion';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_pdf, rscode, resultId);
	call othernameutils2.setothername(objTypeId_pdf, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_pdf, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_pdf;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;


	objTypeName := 'お気に入り';
	objTypeOtherName := 'Favorite';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_favorite, rscode, resultId);
	call othernameutils2.setothername(objTypeId_favorite, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_favorite, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_favorite;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'マイドキュメント';
	objTypeOtherName := 'My document';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_mydoc, rscode, resultId);
	call othernameutils2.setothername(objTypeId_mydoc, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_mydoc, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_mydoc;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '公開通知先エントリー';
	objTypeOtherName := 'Public entry';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_PublicEntry, rscode, resultId);
	call othernameutils2.setothername(objTypeId_PublicEntry, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_PublicEntry, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_PublicEntry;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'PDF結合';
	objTypeOtherName := 'PDF merge';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_PDFMerge, rscode, resultId);
	call othernameutils2.setothername(objTypeId_PDFMerge, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_PDFMerge, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_PDFMerge;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'PDF分割';
	objTypeOtherName := 'PDF split';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_PDFSplit, rscode, resultId);
	call othernameutils2.setothername(objTypeId_PDFSplit, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_PDFSplit, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_PDFSplit;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'PDF署名';
	objTypeOtherName := 'PDF signature';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_PDFSignature, rscode, resultId);
	call othernameutils2.setothername(objTypeId_PDFSignature, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_PDFSignature, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_PDFSignature;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'PDFセキュリティ設定';
	objTypeOtherName := 'PDF security setting';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_PDFSecurity, rscode, resultId);
	call othernameutils2.setothername(objTypeId_PDFSecurity, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_PDFSecurity, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_PDFSecurity;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '署名・暗号化';
	objTypeOtherName := 'Signature and encryption';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_SignAndEncrypt, rscode, resultId);
	call othernameutils2.setothername(objTypeId_SignAndEncrypt, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_SignAndEncrypt, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_SignAndEncrypt;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'タグ';
	objTypeOtherName := 'Tag';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_Tag, rscode, resultId);
	call othernameutils2.setothername(objTypeId_Tag, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_Tag, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_Tag;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '属性表示色';
	objTypeOtherName := 'Value display color';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_ValueDisplayColor, rscode, resultId);
	call othernameutils2.setothername(objTypeId_ValueDisplayColor, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_ValueDisplayColor, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_ValueDisplayColor;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '公開ファイル比較';
	objTypeOtherName := 'Public document comparison';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_DocComapare, rscode, resultId);
	call othernameutils2.setothername(objTypeId_DocComapare, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_DocComapare, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_DocComapare;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '一時格納ファイル';
	objTypeOtherName := 'Temporarily stored file';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_TempStore, rscode, resultId);
	call othernameutils2.setothername(objTypeId_TempStore, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_TempStore, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_TempStore;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'メール通知';
	objTypeOtherName := 'Mail notification';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_MailNotification, rscode, resultId);
	call othernameutils2.setothername(objTypeId_MailNotification, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_MailNotification, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_MailNotification;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'OCR処理';
	objTypeOtherName := 'OCR processing';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_OCR_Processing, rscode, resultId);
	call othernameutils2.setothername(objTypeId_OCR_Processing, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_OCR_Processing, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_OCR_Processing;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := 'URL挿入';
	objTypeOtherName := 'Insert URL';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_InsertURL, rscode, resultId);
	call othernameutils2.setothername(objTypeId_InsertURL, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_InsertURL, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);
	objName := '' || objTypeId_InsertURL;
	call objectdao2.createobject(1, objName, objTypeId_DocumentTypeSecurity, null, 0, 0, null, resultTmpId, resultId, resultCursor, rscode);
	close resultCursor;

	objTypeName := '公開通知テンプレート';
	objTypeOtherName := 'Public notification template';
	call objecttypedao2.createobjecttype(objTypeName, null, null, null, null, objTypeId_publicNotificationTemplate, rscode, resultId);
	call othernameutils2.setothername(objTypeId_publicNotificationTemplate, '', null, 'JA', objTypeName, 6, resultId, rscode);
	call othernameutils2.setothername(objTypeId_publicNotificationTemplate, '', null, 'EN', objTypeOtherName, 6, resultId, rscode);


	/* ===================== */
	/* Create Relation Types */
	/* ===================== */
	relTypeName := 'ドキュメント';
	relTypeOtherName := 'Document';
	call relationtypedao2.createrelationtype(relTypeName, 1, 0, relTypeId, rscode);
	call othernameutils2.setothername(relTypeId, '', null, 'JA', relTypeName, 7, resultId, rscode);
	call othernameutils2.setothername(relTypeId, '', null, 'EN', relTypeOtherName, 7, resultId, rscode);

	relTypeName := 'リンク';
	relTypeOtherName := 'Link';
	call relationtypedao2.createrelationtype(relTypeName, 3, 0, relTypeId_Link, rscode);
	call othernameutils2.setothername(relTypeId_Link, '', null, 'JA', relTypeName, 7, resultId, rscode);
	call othernameutils2.setothername(relTypeId_Link, '', null, 'EN', relTypeOtherName, 7, resultId, rscode);

	relTypeName := 'ブランチ';
	relTypeOtherName := 'Branch';
	call relationtypedao2.createrelationtype(relTypeName, 3, 0, relTypeId_Branch, rscode);
	call othernameutils2.setothername(relTypeId_Branch, '', null, 'JA', relTypeName, 7, resultId, rscode);
	call othernameutils2.setothername(relTypeId_Branch, '', null, 'EN', relTypeOtherName, 7, resultId, rscode);

	relTypeName := 'ごみ箱';
	relTypeOtherName := 'Garbage box';
	call relationtypedao2.createrelationtype(relTypeName, 3, 0, relTypeId_Recycle, rscode);
	call othernameutils2.setothername(relTypeId_Recycle, '', null, 'JA', relTypeName, 7, resultId, rscode);
	call othernameutils2.setothername(relTypeId_Recycle, '', null, 'EN', relTypeOtherName, 7, resultId, rscode);

	/* ====================== */
	/* Create Attribute Types */
	/* ====================== */
	attTypeName := '属性タイプ値マスター数値リスト';
	attTypeOtherName := 'List of attribute type value master numerical value';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_ListOfAttrNum, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrNum, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrNum, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性タイプ値マスター日付値リスト';
	attTypeOtherName := 'List of attribute type value master date values';
	call attributetypedao2.createattributetype(attTypeName, 3, IS_MULTIPLE, 0, attTypeId_ListOfAttrDate, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrDate, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrDate, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性タイプ値マスター文字列値リスト';
	attTypeOtherName := 'List of attribute type value master character string values';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_MULTIPLE, 0, attTypeId_ListOfAttrStr, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrStr, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrStr, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性タイプ値マスターテキスト値リスト';
	attTypeOtherName := 'List of attribute type value master text values';
	call attributetypedao2.createattributetype(attTypeName, 4, IS_MULTIPLE, 0, attTypeId_ListOfAttrText, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrText, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrText, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性タイプ値マスターダブル数字リスト';
	attTypeOtherName := 'List of attribute type value master double values';
	call attributetypedao2.createattributetype(attTypeName, 5, IS_MULTIPLE, 0, attTypeId_ListOfAttrDouble, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrDouble, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrDouble, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性タイプ値マスター表示色リスト';
	attTypeOtherName := 'List of attribute type value master display color values';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_MULTIPLE, 0, attTypeId_ListOfAttrColor, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrColor, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrColor, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性タイプ値マスター表示設定リスト';
	attTypeOtherName := 'List of attribute type value master display setting values';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_ListOfAttrColorSet, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrColorSet, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ListOfAttrColorSet, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性ツリー分類対象';
	attTypeOtherName := 'Attribute tree classification object';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_AttrTreeObject, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreeObject, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreeObject, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性ツリー他言語ID';
	attTypeOtherName := 'Attribute tree disclosure word ID';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_AttrTreeWordID, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreeWordID, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreeWordID, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性ツリー他言語名称';
	attTypeOtherName := 'Attribute tree disclosure word name';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_AttrTreeWordName, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreeWordName, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreeWordName, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性ツリー所属属性ID';
	attTypeOtherName := 'Attribute tree position attribute ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_AttrTreePosAttrID, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreePosAttrID, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreePosAttrID, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性ツリー所属属性ポジション';
	attTypeOtherName := 'Attribute tree position attribute position';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_AttrTreePos, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreePos, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AttrTreePos, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '属性ツリー所属属性値なし表示フラグ';
	attTypeOtherName := 'Indication flag which there is no value in';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_IndicationFlag, rscode);
	call othernameutils2.setothername(attTypeId_IndicationFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_IndicationFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー設定承認依頼先デフォルト設定フラグ';
	attTypeOtherName := 'Workflow setting approval request point default setting flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_DefaultSettingFlag, rscode);
	call othernameutils2.setothername(attTypeId_DefaultSettingFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DefaultSettingFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー設定メール通知方法のデフォルト設定';
	attTypeOtherName := 'Default setting of the workflow setting email notice method';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_EmailNoticeMethod, rscode);
	call othernameutils2.setothername(attTypeId_EmailNoticeMethod, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_EmailNoticeMethod, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー設定差戻し・取戻しメール通知フラグ';
	attTypeOtherName := 'Workflow setting referring a case back to the original court / a recovering email notice flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_EmailNoticeFlag, rscode);
	call othernameutils2.setothername(attTypeId_EmailNoticeFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_EmailNoticeFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー設定処理待ちポップアップ通知フラグ';
	attTypeOtherName := 'Notice of workflow setting processing waiting popup flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_WaitingPopupFlag, rscode);
	call othernameutils2.setothername(attTypeId_WaitingPopupFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_WaitingPopupFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー設定上長承認設定';
	attTypeOtherName := 'Workflow setting boss approval';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_BossApprovalFlag, rscode);
	call othernameutils2.setothername(attTypeId_BossApprovalFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_BossApprovalFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー公開処理PDF変換実施フラグ';
	attTypeOtherName := 'Workflow exhibition processing PDF conversion enforcement flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_PDFConversionFlag, rscode);
	call othernameutils2.setothername(attTypeId_PDFConversionFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PDFConversionFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー公開処理有効期限設定期間数字';
	attTypeOtherName := 'Workflow exhibition processing expiration date setting period number';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_DateSettingPrdNum, rscode);
	call othernameutils2.setothername(attTypeId_DateSettingPrdNum, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DateSettingPrdNum, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー公開処理有効期限設定期間単位';
	attTypeOtherName := 'Workflow exhibition processing expiration date setting period unit';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_DateSettingPrdUnit, rscode);
	call othernameutils2.setothername(attTypeId_DateSettingPrdUnit, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DateSettingPrdUnit, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'パス';
	attTypeOtherName := 'Path';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_MULTIPLE, 0, attTypeId_Path, rscode);
	call othernameutils2.setothername(attTypeId_Path, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Path, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'プロパティ';
	attTypeOtherName := 'Property';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_Property, rscode);
	call othernameutils2.setothername(attTypeId_Property, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Property, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '改訂内容';
	attTypeOtherName := 'Revised content';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_RevUpComment, rscode);
	call othernameutils2.setothername(attTypeId_RevUpComment, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_RevUpComment, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '作成者';
	attTypeOtherName := 'Create user';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_CreateUserId, rscode);
	call othernameutils2.setothername(attTypeId_CreateUserId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_CreateUserId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '有効期限';
	attTypeOtherName := 'Effective term';
	call attributetypedao2.createattributetype(attTypeName, 3, IS_NOT_MULTIPLE, 0, attTypeId_Expire, rscode);
	call othernameutils2.setothername(attTypeId_Expire, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Expire, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'コメント';
	attTypeOtherName := 'Comment';
	call attributetypedao2.createattributetype(attTypeName, 4, IS_NOT_MULTIPLE, 0, attTypeId_Comment, rscode);
	call othernameutils2.setothername(attTypeId_Comment, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Comment, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '受信確認';
	attTypeOtherName := 'Receive confirmation';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_Reply, rscode);
	call othernameutils2.setothername(attTypeId_Reply, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Reply, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '更新者';
	attTypeOtherName := 'Modify user';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_ModifyUserId, rscode);
	call othernameutils2.setothername(attTypeId_ModifyUserId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ModifyUserId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '更新日';
	attTypeOtherName := 'Modify date';
	call attributetypedao2.createattributetype(attTypeName, 3, IS_NOT_MULTIPLE, 0, attTypeId_ModifyDate, rscode);
	call othernameutils2.setothername(attTypeId_ModifyDate, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ModifyDate, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '作成日';
	attTypeOtherName := 'Create date';
	call attributetypedao2.createattributetype(attTypeName, 3, IS_NOT_MULTIPLE, 0, attTypeId_CreateDate, rscode);
	call othernameutils2.setothername(attTypeId_CreateDate, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_CreateDate, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'サイズ';
	attTypeOtherName := 'File size';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_FileSize, rscode);
	call othernameutils2.setothername(attTypeId_FileSize, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_FileSize, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '前セキュリティ';
	attTypeOtherName := 'Previous security';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_PreviousSecurity, rscode);
	call othernameutils2.setothername(attTypeId_PreviousSecurity, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PreviousSecurity, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '上位からの引継ぎ属性';
	attTypeOtherName := 'Transfer attribute from the high rank';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_AttrFromHighRank, rscode);
	call othernameutils2.setothername(attTypeId_AttrFromHighRank, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AttrFromHighRank, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '上位WFフォルダ';
	attTypeOtherName := 'Higher WF folder';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_HigherWFFolder, rscode);
	call othernameutils2.setothername(attTypeId_HigherWFFolder, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_HigherWFFolder, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開処理失敗';
	attTypeOtherName := 'Public processing failure';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_PublicProcFailure, rscode);
	call othernameutils2.setothername(attTypeId_PublicProcFailure, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PublicProcFailure, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '名称割当て属性';
	attTypeOtherName := 'Name allotment attribute';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_NameAllotmentAttr, rscode);
	call othernameutils2.setothername(attTypeId_NameAllotmentAttr, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_NameAllotmentAttr, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '下位への引継ぎ属性';
	attTypeOtherName := 'Transfer attribute to the low rank';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_AttrToLowRank, rscode);
	call othernameutils2.setothername(attTypeId_AttrToLowRank, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AttrToLowRank, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '下位フォルダ管理セキュリティ';
	attTypeOtherName := 'Lower folder management security';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_LowerFolderMngSec, rscode);
	call othernameutils2.setothername(attTypeId_LowerFolderMngSec, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_LowerFolderMngSec, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー設定公開通知先デフォルト設定フラグ';
	attTypeOtherName := 'Workflow setting default notified setting flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_DefaultNotifiedFlag, rscode);
	call othernameutils2.setothername(attTypeId_DefaultNotifiedFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DefaultNotifiedFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー公開処理PDF分割実施フラグ';
	attTypeOtherName := 'Workflow exhibition processing PDF split enforcement flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_PDFSplitFlag, rscode);
	call othernameutils2.setothername(attTypeId_PDFSplitFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PDFSplitFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー公開処理PDF署名実施フラグ';
	attTypeOtherName := 'Workflow exhibition processing PDF signature enforcement flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_PDFSignatureFlag, rscode);
	call othernameutils2.setothername(attTypeId_PDFSignatureFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PDFSignatureFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'エントリータイプID';
	attTypeOtherName := 'Entry type ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_EntryTypeId, rscode);
	call othernameutils2.setothername(attTypeId_EntryTypeId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_EntryTypeId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'エントリー対象ID';
	attTypeOtherName := 'Entry targer ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_EntryTargetId, rscode);
	call othernameutils2.setothername(attTypeId_EntryTargetId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_EntryTargetId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ドキュメントタイプID';
	attTypeOtherName := 'Document type ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_DocumentTypeId, rscode);
	call othernameutils2.setothername(attTypeId_DocumentTypeId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DocumentTypeId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '親オブジェクトID';
	attTypeOtherName := 'Parent object ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_ParentObjectId, rscode);
	call othernameutils2.setothername(attTypeId_ParentObjectId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ParentObjectId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '結合対象オブジェクトID';
	attTypeOtherName := 'Merge target object ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_MergeTargetObjectId, rscode);
	call othernameutils2.setothername(attTypeId_MergeTargetObjectId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_MergeTargetObjectId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '登録者';
	attTypeOtherName := 'Regist user';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_RegistUser, rscode);
	call othernameutils2.setothername(attTypeId_RegistUser, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_RegistUser, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'PDF署名ステータス';
	attTypeOtherName := 'PDF Signature status';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_PDFSignatureStatus, rscode);
	call othernameutils2.setothername(attTypeId_PDFSignatureStatus, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PDFSignatureStatus, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '署名有無';
	attTypeOtherName := 'Signature flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_SignatureFlag, rscode);
	call othernameutils2.setothername(attTypeId_SignatureFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SignatureFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '承認日付挿入';
	attTypeOtherName := 'Insert approve date flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_InsertApproveDate, rscode);
	call othernameutils2.setothername(attTypeId_InsertApproveDate, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_InsertApproveDate, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '承認者名挿入';
	attTypeOtherName := 'Insert approver flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_InsertApproverFlag, rscode);
	call othernameutils2.setothername(attTypeId_InsertApproverFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_InsertApproverFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '挿入ページ';
	attTypeOtherName := 'Insert page';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_InsertPage, rscode);
	call othernameutils2.setothername(attTypeId_InsertPage, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_InsertPage, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '挿入位置基準点';
	attTypeOtherName := 'Insert place';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_InsertPlace, rscode);
	call othernameutils2.setothername(attTypeId_InsertPlace, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_InsertPlace, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '挿入位置座標X';
	attTypeOtherName := 'Insert place X';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_InsertPlaceX, rscode);
	call othernameutils2.setothername(attTypeId_InsertPlaceX, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_InsertPlaceX, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '挿入位置座標Y';
	attTypeOtherName := 'Insert place Y';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_InsertPlaceY, rscode);
	call othernameutils2.setothername(attTypeId_InsertPlaceY, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_InsertPlaceY, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'セキュリティ設定有無';
	attTypeOtherName := 'Setting security flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_SetSecurityFlag, rscode);
	call othernameutils2.setothername(attTypeId_SetSecurityFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SetSecurityFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'セキュリティパスワード設定有無';
	attTypeOtherName := 'Setting security password flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_SetSecurityPWFlag, rscode);
	call othernameutils2.setothername(attTypeId_SetSecurityPWFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SetSecurityPWFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'セキュリティパスワード';
	attTypeOtherName := 'Security Password';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_SecurityPassword, rscode);
	call othernameutils2.setothername(attTypeId_SecurityPassword, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SecurityPassword, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '参照用パスワード設定有無';
	attTypeOtherName := 'Setting read password flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_SetReadPWFlag, rscode);
	call othernameutils2.setothername(attTypeId_SetReadPWFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SetReadPWFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '参照用パスワード';
	attTypeOtherName := 'Read Password';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_ReadPassword, rscode);
	call othernameutils2.setothername(attTypeId_ReadPassword, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ReadPassword, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '印刷許可設定';
	attTypeOtherName := 'Accept to print flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_AcceptPrintFlag, rscode);
	call othernameutils2.setothername(attTypeId_AcceptPrintFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AcceptPrintFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '編集許可設定';
	attTypeOtherName := 'Accept to edit flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_AcceptEditFlag, rscode);
	call othernameutils2.setothername(attTypeId_AcceptEditFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AcceptEditFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '注釈追加許可設定';
	attTypeOtherName := 'Accept to add note flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_AcceptAddNoteFlag, rscode);
	call othernameutils2.setothername(attTypeId_AcceptAddNoteFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AcceptAddNoteFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '転載許可設定';
	attTypeOtherName := 'Accept to reprint flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_AcceptReprintFlag, rscode);
	call othernameutils2.setothername(attTypeId_AcceptReprintFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AcceptReprintFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'PDF結合処理失敗';
	attTypeOtherName := 'Failed to PDF merge';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_FailedPDFMerge, rscode);
	call othernameutils2.setothername(attTypeId_FailedPDFMerge, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_FailedPDFMerge, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'PDF分割状態';
	attTypeOtherName := 'PDF split status';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_PDFSplitStatus, rscode);
	call othernameutils2.setothername(attTypeId_PDFSplitStatus, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PDFSplitStatus, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'リンク先';
	attTypeOtherName := 'Target to link';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_TargetToLink, rscode);
	call othernameutils2.setothername(attTypeId_TargetToLink, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_TargetToLink, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ドキュメントリンク';
	attTypeOtherName := 'Document link';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_DocumentLink, rscode);
	call othernameutils2.setothername(attTypeId_DocumentLink, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DocumentLink, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ブランチ先削除';
	attTypeOtherName := 'Delete branch';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_DeleteBranchFlag, rscode);
	call othernameutils2.setothername(attTypeId_DeleteBranchFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DeleteBranchFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '対象ドキュメント';
	attTypeOtherName := 'Target document';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_TargetDocument, rscode);
	call othernameutils2.setothername(attTypeId_TargetDocument, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_TargetDocument, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '実施者';
	attTypeOtherName := 'Operator';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_Operator, rscode);
	call othernameutils2.setothername(attTypeId_Operator, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Operator, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '選択タグ';
	attTypeOtherName := 'Selected tag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_SelectedTag, rscode);
	call othernameutils2.setothername(attTypeId_SelectedTag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SelectedTag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'タグ';
	attTypeOtherName := 'Tag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_Tag, rscode);
	call othernameutils2.setothername(attTypeId_Tag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Tag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'タグ付与者';
	attTypeOtherName := 'Tag giver';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_TagGiver, rscode);
	call othernameutils2.setothername(attTypeId_TagGiver, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_TagGiver, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'タグ付与日';
	attTypeOtherName := 'Tag given date';
	call attributetypedao2.createattributetype(attTypeName, 3, IS_MULTIPLE, 0, attTypeId_TagGivenDate, rscode);
	call othernameutils2.setothername(attTypeId_TagGivenDate, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_TagGivenDate, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '署名・暗号化状態';
	attTypeOtherName := 'Signature and encryption status';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_SignEncryptStatus, rscode);
	call othernameutils2.setothername(attTypeId_SignEncryptStatus, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SignEncryptStatus, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '署名・暗号化バージョン';
	attTypeOtherName := 'Signature and encryption version';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_SignEncryptVer, rscode);
	call othernameutils2.setothername(attTypeId_SignEncryptVer, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SignEncryptVer, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '表示色';
	attTypeOtherName := 'Display color';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_DisplayColor, rscode);
	call othernameutils2.setothername(attTypeId_DisplayColor, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DisplayColor, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '比較元ドキュメントオブジェクトID';
	attTypeOtherName := 'Source comparison document';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_SourceCompDoc, rscode);
	call othernameutils2.setothername(attTypeId_SourceCompDoc, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SourceCompDoc, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '比較対象ドキュメントオブジェクトID';
	attTypeOtherName := 'Destination comparison document';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_DestCompDoc, rscode);
	call othernameutils2.setothername(attTypeId_DestCompDoc, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DestCompDoc, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '完了通知メール設定';
	attTypeOtherName := 'Notification mail flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_NotifMailFlag, rscode);
	call othernameutils2.setothername(attTypeId_NotifMailFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_NotifMailFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'レイアウト解析設定';
	attTypeOtherName := 'Analyze layout flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_AnalyzeLayoutFlag, rscode);
	call othernameutils2.setothername(attTypeId_AnalyzeLayoutFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AnalyzeLayoutFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ZIPファイル名';
	attTypeOtherName := 'ZIP name';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_ZIPFileName, rscode);
	call othernameutils2.setothername(attTypeId_ZIPFileName, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ZIPFileName, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '登録先パス';
	attTypeOtherName := 'Registory Path';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_ZipRegistoryPath, rscode);
	call othernameutils2.setothername(attTypeId_ZipRegistoryPath, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ZipRegistoryPath, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知タイミング';
	attTypeOtherName := 'Notification of opening to the public timing';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_NotificationOfTiming, rscode);
	call othernameutils2.setothername(attTypeId_NotificationOfTiming, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_NotificationOfTiming, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '承認依頼通知タイミング';
	attTypeOtherName := 'Approval request notification timing';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_ApprovalReqTiming, rscode);
	call othernameutils2.setothername(attTypeId_ApprovalReqTiming, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ApprovalReqTiming, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知送信先';
	attTypeOtherName := 'Notification of opening to the public destination';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_MULTIPLE, 0, attTypeId_NotificationOfDest, rscode);
	call othernameutils2.setothername(attTypeId_NotificationOfDest, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_NotificationOfDest, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '使用可能ドキュメントタイプ絞込みフラグ';
	attTypeOtherName := 'Limit document type flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_LimitDocTypeFlag, rscode);
	call othernameutils2.setothername(attTypeId_LimitDocTypeFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_LimitDocTypeFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '使用可能ドキュメントタイプ';
	attTypeOtherName := 'Selectable document type';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_SelectableDocType, rscode);
	call othernameutils2.setothername(attTypeId_SelectableDocType, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SelectableDocType, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '使用可能フォルダタイプ絞込みフラグ';
	attTypeOtherName := 'Limit folder type flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_LimitFolderTypeFlag, rscode);
	call othernameutils2.setothername(attTypeId_LimitFolderTypeFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_LimitFolderTypeFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '使用可能フォルダタイプ';
	attTypeOtherName := 'Selectable folder type';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_SelectableFolderType, rscode);
	call othernameutils2.setothername(attTypeId_SelectableFolderType, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SelectableFolderType, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '使用可能タグタイプ絞込みフラグ';
	attTypeOtherName := 'Limit tag type flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_LimitTagTypeFlag, rscode);
	call othernameutils2.setothername(attTypeId_LimitTagTypeFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_LimitTagTypeFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '使用可能タグタイプ';
	attTypeOtherName := 'Selectable tag type';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_SelectableTagType, rscode);
	call othernameutils2.setothername(attTypeId_SelectableTagType, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SelectableTagType, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '使用可能セキュリティ絞込みフラグ';
	attTypeOtherName := 'Limit Security flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_LimitSecurityFlag, rscode);
	call othernameutils2.setothername(attTypeId_LimitSecurityFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_LimitSecurityFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '使用可能セキュリティ';
	attTypeOtherName := 'Selectable security';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_SelectableSecurity, rscode);
	call othernameutils2.setothername(attTypeId_SelectableSecurity, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SelectableSecurity, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '責任者';
	attTypeOtherName := 'Administrator';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_Administrator, rscode);
	call othernameutils2.setothername(attTypeId_Administrator, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Administrator, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '責任者種別';
	attTypeOtherName := 'Administrator type';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_AdministratorType, rscode);
	call othernameutils2.setothername(attTypeId_AdministratorType, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_AdministratorType, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'リンク更新タイミング';
	attTypeOtherName := 'Update Timing Linked';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_linkUpdateTiming, rscode);
	call othernameutils2.setothername(attTypeId_linkUpdateTiming, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_linkUpdateTiming, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'OCR処理ステータス';
	attTypeOtherName := 'OCR processing status';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_OCR_Status, rscode);
	call othernameutils2.setothername(attTypeId_OCR_Status, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_OCR_Status, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'OCR結果ステータス';
	attTypeOtherName := 'OCR result status';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_OCR_Result_Status, rscode);
	call othernameutils2.setothername(attTypeId_OCR_Result_Status, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_OCR_Result_Status, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'OCR設定有無';
	attTypeOtherName := 'OCR setting existence';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_OCR_Setting, rscode);
	call othernameutils2.setothername(attTypeId_OCR_Setting, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_OCR_Setting, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'チェックイン可能ステータス';
	attTypeOtherName := 'Status enable to checkin by approver';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_ApproverCheckin, rscode);
	call othernameutils2.setothername(attTypeId_ApproverCheckin, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ApproverCheckin, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'WebDAVロックフラグ';
	attTypeOtherName := 'WebDAV lock flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_WebDAV_lock_flag, rscode);
	call othernameutils2.setothername(attTypeId_WebDAV_lock_flag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_WebDAV_lock_flag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '番号';
	attTypeOtherName := 'Number';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_Number, rscode);
	call othernameutils2.setothername(attTypeId_Number, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Number, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'リビジョンアップ引継ぎ';
	attTypeOtherName := 'Revision up takeover';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_RevisionUpTakeover, rscode);
	call othernameutils2.setothername(attTypeId_RevisionUpTakeover, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_RevisionUpTakeover, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '最新リビジョン関連付け';
	attTypeOtherName := 'Latest revision association';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_LatestRevAssociation, rscode);
	call othernameutils2.setothername(attTypeId_LatestRevAssociation, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_LatestRevAssociation, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '選択カスタマイズテーブル名称';
	attTypeOtherName := 'Select Custom Table Name';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_Select_Custom_Table, rscode);
	call othernameutils2.setothername(attTypeId_Select_Custom_Table, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_Select_Custom_Table, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ワークフロー公開処理URL挿入フラグ';
	attTypeOtherName := 'Workflow exhibition processing insert URL flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_InsertURLFlag, rscode);
	call othernameutils2.setothername(attTypeId_InsertURLFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_InsertURLFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '文書ID';
	attTypeOtherName := 'Document Search Index';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_SearchIndex, rscode);
	call othernameutils2.setothername(attTypeId_SearchIndex, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SearchIndex, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '関連ドキュメント';
	attTypeOtherName := 'Related document';
	call attributetypedao2.createattributetype(attTypeName, 6, IS_MULTIPLE, 0, attTypeId_DocumentAttachment, rscode);
	call othernameutils2.setothername(attTypeId_DocumentAttachment, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DocumentAttachment, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '添付ファイル';
	attTypeOtherName := 'Packaged document';
	call attributetypedao2.createattributetype(attTypeName, 6, IS_MULTIPLE, 0, attTypeId_TemporaryAttachment, rscode);
	call othernameutils2.setothername(attTypeId_TemporaryAttachment, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_TemporaryAttachment, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知コメント';
	attTypeOtherName := 'Public notification comment';
	call attributetypedao2.createattributetype(attTypeName, 4, IS_NOT_MULTIPLE, 0, attTypeId_PublicComment, rscode);
	call othernameutils2.setothername(attTypeId_PublicComment, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PublicComment, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知コメントログ';
	attTypeOtherName := 'Public notification comment log';
	call attributetypedao2.createattributetype(attTypeName, 4, IS_NOT_MULTIPLE, 0, attTypeId_PublicCommentLog, rscode);
	call othernameutils2.setothername(attTypeId_PublicCommentLog, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PublicCommentLog, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '削除日時';
	attTypeOtherName := 'Delete Date';
	call attributetypedao2.createattributetype(attTypeName, 3, IS_NOT_MULTIPLE, 0, attTypeId_DeleteDate, rscode);
	call othernameutils2.setothername(attTypeId_DeleteDate, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_DeleteDate, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '上長のみ表示デフォルト設定ステータス';
	attTypeOtherName := 'Boss only default setting status';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_BossOnlyDefaultFlag, rscode);
	call othernameutils2.setothername(attTypeId_BossOnlyDefaultFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_BossOnlyDefaultFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'PDF変換処理実行日時';
	attTypeOtherName := 'PDF Conversion Exec Date';
	call attributetypedao2.createattributetype(attTypeName, 3, IS_NOT_MULTIPLE, 0, attTypeId_PDFConversionExecDate, rscode);
	call othernameutils2.setothername(attTypeId_PDFConversionExecDate, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PDFConversionExecDate, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開PDF事前登録日時';
	attTypeOtherName := 'Public PDF pre-regist date';
	call attributetypedao2.createattributetype(attTypeName, 3, IS_NOT_MULTIPLE, 0, attTypeId_publicPDFPreRegistDate, rscode);
	call othernameutils2.setothername(attTypeId_publicPDFPreRegistDate, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_publicPDFPreRegistDate, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'スキップステータスタイプID';
	attTypeOtherName := 'Skip StatusType ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_SkipStatus, rscode);
	call othernameutils2.setothername(attTypeId_SkipStatus, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SkipStatus, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '電子署名用言語';
	attTypeOtherName := 'Approve name lang';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_ApproveNameLang, rscode);
	call othernameutils2.setothername(attTypeId_ApproveNameLang, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ApproveNameLang, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '署名用ジョブ名';
	attTypeOtherName := 'Sign job name';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_SignJobName, rscode);
	call othernameutils2.setothername(attTypeId_SignJobName, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_SignJobName, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知テンプレート名称';
	attTypeOtherName := 'Public notification template name';
	call attributetypedao2.createattributetype(attTypeName, 2, IS_NOT_MULTIPLE, 0, attTypeId_PublicNotificationTemplateName, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateName, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateName, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知テンプレートユーザID';
	attTypeOtherName := 'Public notification template user ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_PublicNotificationTemplateUserId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateUserId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateUserId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知テンプレートグループID';
	attTypeOtherName := 'Public notification template group ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_PublicNotificationTemplateGroupId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateGroupId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateGroupId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知テンプレートロールID';
	attTypeOtherName := 'Public notification template role ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_PublicNotificationTemplateRoleId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateRoleId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateRoleId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '公開通知テンプレート複合グループID';
	attTypeOtherName := 'Public notification template composite group ID';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_MULTIPLE, 0, attTypeId_PublicNotificationTemplateCompositeGroupId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateCompositeGroupId, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PublicNotificationTemplateCompositeGroupId, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := '手動削除禁止フラグ';
	attTypeOtherName := 'Manual deletion prohibited Flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, 0, attTypeId_ManualDeletionProhibitedFlag, rscode);
	call othernameutils2.setothername(attTypeId_ManualDeletionProhibitedFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ManualDeletionProhibitedFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'ユーザ別Box連携利用許可フラグ';
	attTypeOtherName := 'User Box Integration Flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, null, attTypeId_UserBoxIntegrationFlag, rscode);
	call othernameutils2.setothername(attTypeId_UserBoxIntegrationFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_UserBoxIntegrationFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'サムネイル変換不可フラグ';
	attTypeOtherName := 'Thumbnail Conversion Disabled Flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, null, attTypeId_ThumbnailConversionDisabledFlag, rscode);
	call othernameutils2.setothername(attTypeId_ThumbnailConversionDisabledFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_ThumbnailConversionDisabledFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	attTypeName := 'プレビュー変換不可フラグ';
	attTypeOtherName := 'Preview Conversion Disabled Flag';
	call attributetypedao2.createattributetype(attTypeName, 1, IS_NOT_MULTIPLE, null, attTypeId_PreviewConversionDisabledFlag, rscode);
	call othernameutils2.setothername(attTypeId_PreviewConversionDisabledFlag, '', null, 'JA', attTypeName, 4, resultId, rscode);
	call othernameutils2.setothername(attTypeId_PreviewConversionDisabledFlag, '', null, 'EN', attTypeOtherName, 4, resultId, rscode);

	/* ============================================= */
	/* Apply Object Type To AttributeTypeValueMaster */
	/* ============================================= */
	call objecttypedao2.addattributetype(objTypeId_AttrValueMaster, null, attTypeId_ListOfAttrNum, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrValueMaster, null, attTypeId_ListOfAttrDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrValueMaster, null, attTypeId_ListOfAttrStr, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrValueMaster, null, attTypeId_ListOfAttrText, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrValueMaster, null, attTypeId_ListOfAttrDouble, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrValueMaster, null, attTypeId_ListOfAttrColor, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrValueMaster, null, attTypeId_ListOfAttrColorSet, null, rscode, resultId);

	/* ================================== */
	/* Apply Object Type To AttributeTree */
	/* ================================== */
	call objecttypedao2.addattributetype(objTypeId_AttrTree, null, attTypeId_AttrTreeObject, null, rscode, resultId);

	/* ================================================ */
	/* Apply Object Type To AttributeTreeDisclosureWord */
	/* ================================================ */
	call objecttypedao2.addattributetype(objTypeId_AttrDisclosureWord, null, attTypeId_AttrTreeWordID, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrDisclosureWord, null, attTypeId_AttrTreeWordName, null, rscode, resultId);

	/* =================================================== */
	/* Apply Object Type To AttributeTreePositionAttribute */
	/* =================================================== */
	call objecttypedao2.addattributetype(objTypeId_AttrPositionAttr, null, attTypeId_AttrTreePosAttrID, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrPositionAttr, null, attTypeId_AttrTreePos, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_AttrPositionAttr, null, attTypeId_IndicationFlag, null, rscode, resultId);

	/* ==================================== */
	/* Apply Object Type To WorkflowSetting */
	/* ==================================== */
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_DefaultSettingFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_DefaultNotifiedFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_EmailNoticeMethod, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_EmailNoticeFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_WaitingPopupFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_BossApprovalFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_OCR_Setting, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_ApproverCheckin, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowSetting, null, attTypeId_BossOnlyDefaultFlag, null, rscode, resultId);

	/* ================================================= */
	/* Apply Object Type To WorkflowExhibitionProcessing */
	/* ================================================= */
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_PDFConversionFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_DateSettingPrdNum, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_DateSettingPrdUnit, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_PDFSplitFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_PDFSignatureFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_SignatureFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_InsertApproveDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_InsertApproverFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_InsertPage, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_InsertPlace, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_InsertPlaceX, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_InsertPlaceY, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_SetSecurityFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_SetSecurityPWFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_SecurityPassword, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_SetReadPWFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_ReadPassword, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_AcceptPrintFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_AcceptEditFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_AcceptAddNoteFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_AcceptReprintFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_InsertURLFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_ApproveNameLang, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkflowExhibition, null, attTypeId_SignJobName, null, rscode, resultId);

	/* ============================ */
	/* Apply Object Type To Document*/
	/* ============================ */
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_Path, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_Property, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_RevUpComment, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_CreateUserId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_Expire, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_ModifyUserId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_ModifyDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_CreateDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_FileSize, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_PreviousSecurity, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_AttrFromHighRank, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_HigherWFFolder, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_PublicProcFailure, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_FailedPDFMerge, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_PDFSplitStatus, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_TargetToLink, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_DocumentLink, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_Tag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_TagGiver, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_TagGivenDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_SignEncryptStatus, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_SignEncryptVer, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_OCR_Status, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_OCR_Result_Status, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_WebDAV_lock_flag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_Number, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_SearchIndex, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_DeleteDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_PDFConversionExecDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_publicPDFPreRegistDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_SkipStatus, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_ThumbnailConversionDisabledFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Document, null, attTypeId_PreviewConversionDisabledFlag, null, rscode, resultId);

	/* ============================== */
	/* Apply Object Type To WorkSpace */
	/* ============================== */
	call objecttypedao2.addattributetype(objTypeId_WorkSpace, null, attTypeId_Path, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkSpace, null, attTypeId_Property, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkSpace, null, attTypeId_NameAllotmentAttr, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkSpace, null, attTypeId_AttrToLowRank, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_WorkSpace, null, attTypeId_LowerFolderMngSec, null, rscode, resultId);

	/* =========================== */
	/* Apply Object Type To Folder */
	/* =========================== */
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_AttrFromHighRank, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_Expire, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_PreviousSecurity, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_HigherWFFolder, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_Tag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_TagGiver, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_TagGivenDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_DeleteDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_PublicProcFailure, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Folder, null, attTypeId_SkipStatus, null, rscode, resultId);

	/* ========================================= */
	/* Apply Object Type To WorkSpace Garbagebox */
	/* ========================================= */
	call objecttypedao2.addattributetype(objTypeId_WorkspaceRecycle, null, attTypeId_Path, null, rscode, resultId);

	/* ================================ */
	/* Apply Object Type To PublicEntry */
	/* ================================ */
	call objecttypedao2.addattributetype(objTypeId_PublicEntry, null, attTypeId_EntryTypeId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PublicEntry, null, attTypeId_EntryTargetId, null, rscode, resultId);

	/* ============================= */
	/* Apply Object Type To PDFMerge */
	/* ============================= */
	call objecttypedao2.addattributetype(objTypeId_PDFMerge, null, attTypeId_DocumentTypeId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFMerge, null, attTypeId_ParentObjectId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFMerge, null, attTypeId_MergeTargetObjectId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFMerge, null, attTypeId_RegistUser, null, rscode, resultId);

	/* ================================= */
	/* Apply Object Type To PDFSignature */
	/* ================================= */
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_PDFSignatureStatus, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_SignatureFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_InsertApproveDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_InsertApproverFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_InsertPage, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_InsertPlace, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_InsertPlaceX, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_InsertPlaceY, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_SetSecurityFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_SetSecurityPWFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_SecurityPassword, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_SetReadPWFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_ReadPassword, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_AcceptPrintFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_AcceptEditFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_AcceptAddNoteFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSignature, null, attTypeId_AcceptReprintFlag, null, rscode, resultId);

	/* ================================ */
	/* Apply Object Type To PDFSecurity */
	/* ================================ */
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_RegistUser, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_SetSecurityFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_SetSecurityPWFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_SecurityPassword, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_SetReadPWFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_ReadPassword, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_AcceptPrintFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_AcceptEditFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_AcceptAddNoteFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_PDFSecurity, null, attTypeId_AcceptReprintFlag, null, rscode, resultId);

	/* ============================================= */
	/* Apply Object Type To Signature and Encryption */
	/* ============================================= */
	call objecttypedao2.addattributetype(objTypeId_SignAndEncrypt, null, attTypeId_TargetDocument, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_SignAndEncrypt, null, attTypeId_Operator, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_SignAndEncrypt, null, attTypeId_SelectedTag, null, rscode, resultId);

	/* ======================== */
	/* Apply Object Type To Tag */
	/* ======================== */
	call objecttypedao2.addattributetype(objTypeId_Tag, null, attTypeId_Path, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Tag, null, attTypeId_Property, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Tag, null, attTypeId_CreateUserId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Tag, null, attTypeId_Expire, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Tag, null, attTypeId_Tag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Tag, null, attTypeId_TagGiver, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Tag, null, attTypeId_TagGivenDate, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Tag, null, attTypeId_SignEncryptStatus, null, rscode, resultId);

	/* ======================================== */
	/* Apply Object Type To Value Display Color */
	/* ======================================== */
	call objecttypedao2.addattributetype(objTypeId_ValueDisplayColor, null, attTypeId_DisplayColor, null, rscode, resultId);

	/* =============================================== */
	/* Apply Object Type To Public document comparison */
	/* =============================================== */
	call objecttypedao2.addattributetype(objTypeId_DocComapare, null, attTypeId_SourceCompDoc, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_DocComapare, null, attTypeId_DestCompDoc, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_DocComapare, null, attTypeId_NotifMailFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_DocComapare, null, attTypeId_AnalyzeLayoutFlag, null, rscode, resultId);

	/* ============================================= */
	/* Apply Object Type To Temporarily stored file */
	/* ============================================= */
	call objecttypedao2.addattributetype(objTypeId_TempStore, null, attTypeId_ZIPFileName, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_TempStore, null, attTypeId_ZipRegistoryPath, null, rscode, resultId);

	/* ============================================= */
	/* Apply Object Type To Mail notification */
	/* ============================================= */
	call objecttypedao2.addattributetype(objTypeId_MailNotification, null, attTypeId_NotificationOfTiming, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_MailNotification, null, attTypeId_ApprovalReqTiming, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_MailNotification, null, attTypeId_Reply, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_MailNotification, null, attTypeId_NotificationOfDest, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_MailNotification, null, attTypeId_PublicComment, null, rscode, resultId);

	/* ============================================= */
	/* Apply Object Type To Workspace management     */
	/* ============================================= */
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_LimitDocTypeFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_SelectableDocType, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_LimitFolderTypeFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_SelectableFolderType, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_LimitTagTypeFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_SelectableTagType, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_LimitSecurityFlag, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_SelectableSecurity, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_Administrator, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_workspace, null, attTypeId_AdministratorType, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_Workspace, null, attTypeId_ManualDeletionProhibitedFlag, null, rscode, resultId);
	
	/* ============================================= */
	/* Apply Object Type To Object Type Object       */
	/* ============================================= */
	select id into objTypeId_ObjectType from EIMOBJTYPE where name = 'オブジェクトタイプ';
	call objecttypedao2.addattributetype(objTypeId_ObjectType, null, attTypeId_RevisionUpTakeover, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_ObjectType, null, attTypeId_LatestRevAssociation, null, rscode, resultId);

	/* ============================================= */
	/* Apply Object Type To User Type       */
	/* ============================================= */
	select id into objTypeId_UserObjectType from EIMOBJTYPE where name = 'ユーザ';
	call objecttypedao2.addattributetype(objTypeId_UserObjectType, null, attTypeId_Select_Custom_Table, null, rscode, resultId);
	select current_setting('psql.default_UserBoxIntegrationFlag',false) into strict default_UserBoxIntegrationFlag;
	call objecttypedao2.addattributetype(objTypeId_UserObjectType, null, attTypeId_UserBoxIntegrationFlag, null, rscode, resultId);
	call attributetypedao2.setdefaultvaluelong(attTypeId_UserBoxIntegrationFlag, null, 0, default_UserBoxIntegrationFlag, 0, rscode, resultId);

	/* ============================================= */
	/* Apply Object Type To publicNotificationTemplate Type       */
	/* ============================================= */
	call objecttypedao2.addattributetype(objTypeId_publicNotificationTemplate, null, attTypeId_PublicNotificationTemplateName, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_publicNotificationTemplate, null, attTypeId_PublicNotificationTemplateUserId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_publicNotificationTemplate, null, attTypeId_PublicNotificationTemplateGroupId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_publicNotificationTemplate, null, attTypeId_PublicNotificationTemplateRoleId, null, rscode, resultId);
	call objecttypedao2.addattributetype(objTypeId_publicNotificationTemplate, null, attTypeId_PublicNotificationTemplateCompositeGroupId, null, rscode, resultId);

	/* ============================= */
	/* Apply Relation Type To Branch */
	/* ============================= */
	call relationtypedao2.addattributetype(relTypeId_Branch, null, attTypeId_DeleteBranchFlag, null, rscode);

	/* ============================= */
	/* Apply Relation Type To Link */
	/* ============================= */
	call relationtypedao2.addattributetype(relTypeId_Link, null, attTypeId_linkUpdateTiming, null, rscode);

	/* ============================== */
	/* Insert Application Access Role */
	/* ============================== */
	insert into EIMACRTYPE values(500, 'ROLE_500');
	insert into EIMACRTYPEOTHER values(500, 'JA', '常時読取');
	insert into EIMACRTYPEOTHER values(500, 'EN', 'Always read');

	/* ====================== */
	/* Create System Security */
	/* ====================== */

	secName := 'system';
	call securitydao2.createsecurity(secName, secId, rscode);
	call accessentrydao2.createaccessentry(secId, null, 1, 1, 1, entryId, rscode);
	call accessentrydao2.setaccessrole(entryId, 11, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 12, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 13, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 14, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 15, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 21, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 22, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 31, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 32, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 41, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 42, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 51, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 61, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 62, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 63, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 101, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 500, null, 1, rscode);

	/* 言語＝日本語 */
	insert into EIMSECOTHER values(secId, 'JA', 'system');
	/* 言語＝英語 */
	insert into EIMSECOTHER values(secId, 'EN', 'system');

	/* ===================== */
	/* Create Recycle Object */
	/* ===================== */
	objName := 'ごみ箱';
	call objectdao2.createobject(1, objName, objTypeId_Recycle, null, 0, 0, null, resultTmpId, objId, resultCursor, rscode);
	close resultCursor;

	/* ゴミ箱自体にはセキュリティは設定しない。 */
	/*systemセキュリティオブジェクトを作成*/
	objName := 'system';
	call objectdao2.createobject(1, objName, objTypeId_SystemSecurity, null, 0, secId, null, resultTmpId, objId, resultCursor, rscode);

	/* ==================================== */
	/* Create Workspace Garbagebox Security */
	/* ==================================== */

	secName := 'ワークスペース固有ごみ箱セキュリティ';
	call securitydao2.createsecurity(secName, secId, rscode);
	call accessentrydao2.createaccessentry(secId, null, -12000, 5, 1, entryId, rscode);
	call accessentrydao2.setaccessrole(entryId, 11, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 12, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 13, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 14, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 15, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 21, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 22, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 31, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 32, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 41, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 42, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 51, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 61, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 62, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 63, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 101, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 500, null, 1, rscode);

	/* 言語＝日本語 */
	insert into EIMSECOTHER values(secId, 'JA', 'ワークスペース固有ごみ箱セキュリティ');
	/* 言語＝英語 */
	insert into EIMSECOTHER values(secId, 'EN', 'Workspace Garbagebox Security');

	/* ============================================= */
	/* Create Workspace Garbagebox Contents Security */
	/* ============================================= */

	secName := 'ワークスペース固有ごみ箱配下セキュリティ';
	call securitydao2.createsecurity(secName, secId, rscode);
	call accessentrydao2.createaccessentry(secId, null, -12001, 5, 1, entryId, rscode);
	call accessentrydao2.setaccessrole(entryId, 11, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 12, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 13, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 14, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 15, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 21, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 22, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 31, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 32, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 41, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 42, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 51, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 61, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 62, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 63, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 101, null, 1, rscode);
	call accessentrydao2.setaccessrole(entryId, 500, null, 1, rscode);

	/* 言語＝日本語 */
	insert into EIMSECOTHER values(secId, 'JA', 'ワークスペース固有ごみ箱配下セキュリティ');
	/* 言語＝英語 */
	insert into EIMSECOTHER values(secId, 'EN', 'Workspace Garbagebox Contents Security');




	/* ======================== */
	/* Update System User Admin */
	/* ======================== */
	update EIMUser set ADMIN = '39935' where ID = '1';

	/* ========================================= */
	/* Set System User UserBoxIntegrationFlag */
	/* ========================================= */
	select id into STRICT objId_system from EIMOBJ where name = '1';
	call AttributeUtils2.setAttributelong(1, objId_system, attTypeId_UserBoxIntegrationFlag, null, 0, default_UserBoxIntegrationFlag, 1, 1, 0, resultCursor, rscode, errorValue);


exception
	when OTHERS then
		RAISE INFO 'SQLSTATE = %', SQLSTATE;
		RAISE INFO 'SQLERRM = %', SQLERRM;
		ROLLBACK;
		RETURN;

end$$
;

