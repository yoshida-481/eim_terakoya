package eim.command.common.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;

import common.util.AppConstant;
import common.util.AppLogicUtil;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.DisplayColorUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMFormat;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.SearchConditionBuildHelper;
import eim.bo.EIMSecurity;
import eim.command.business.service.EIMCommandService;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.WorkFlowUtils;

public class DocumentManagementUtils {
	
	private EIMSession sess = null;
	
	private final String OBJECT_TYPE_NAME_DOCUMENT		= "ドキュメント";
	private final String OBJECT_TYPE_NAME_WORKSPACE		= "ワークスペース";
	private final String OBJECT_TYPE_NAME_TAG					= "タグ";
	
	public DocumentManagementUtils(EIMSession sess) {
		this.sess = sess;
	}
	
	@SuppressWarnings("unchecked")
	public EIMObject getFolderByPathAndName(String path, String name) throws Exception {
		
		if(path.equals("/")) {
			
			EIMObjectType objTypeWorkSpace = ObjectUtils.getObjectTypeByName(sess, "ワークスペース");
			
			EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			
			selectTarget.setCondition(h.group(h.opAnd())
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeWorkSpace.getId()))
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name))
				.addCondition(h.latest(h.opAnd())));
			
			List<EIMObject> objList = SearchUtils.searchObjects(sess, selectTarget, null);
			if(objList == null || objList.size() == 0) {
				return null;
			}
			
			return objList.get(0);
			
			
			
		} else {
			
			EIMObjectType objTypeFolder = ObjectUtils.getObjectTypeByName(sess, "フォルダ");
			EIMAttributeType attTypePath = AttributeUtils.getAttributeTypeByName(sess, "パス");
			
			EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			
			selectTarget.setCondition(h.group(h.opAnd())
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeFolder.getId()))
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name))
				.addCondition(h.eq(h.opAnd(), attTypePath, path))
				.addCondition(h.latest(h.opAnd())));
			
			List<EIMObject> objList = SearchUtils.searchObjects(sess, selectTarget, null);
			if(objList == null || objList.size() == 0) {
				return null;
			}
			
			return objList.get(0);
			
		}
		
	}
	
	@SuppressWarnings("unchecked")
	public List<EIMObject> getDocumentListByPathAndName(String path, String name) throws Exception {
		
		EIMObjectType objTypeDocument = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
		EIMAttributeType attTypePath = AttributeUtils.getAttributeTypeByName(sess, "パス");
		
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		SearchConditionBuildHelper h = new SearchConditionBuildHelper();
		
		selectTarget.setCondition(h.group(h.opAnd())
			.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeDocument.getId()))
			.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name))
			.addCondition(h.eq(h.opAnd(), attTypePath, path))
			.addCondition(h.latest(h.opAnd())));
		
		List<EIMObject> objList = SearchUtils.searchObjects(sess, selectTarget, null);
		return objList;
		
	}
	
	public EIMObject getDocumentByPathAndName(String path, String name) throws Exception {
		
		List<EIMObject> objList = this.getDocumentListByPathAndName(path, name);
		if(objList == null || objList.size() == 0) {
			return null;
		}
		
		return objList.get(0);
		
	}
	
	@SuppressWarnings("unchecked")
	public List<EIMObject> getLatestObjectListByPathAndName(String path, String name) throws Exception {
		
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		if(path.equals("/")) {
			
			EIMObjectType objTypeWorkSpace = ObjectUtils.getObjectTypeByName(sess, "ワークスペース");
			
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			
			selectTarget.setCondition(h.group(h.opAnd())
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeWorkSpace.getId()))
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name))
				.addCondition(h.latest(h.opAnd())));
		} else {
			
			//EIMObjectType objTypeFolder = ObjectUtils.getObjectTypeByName(sess, "フォルダ");
			EIMAttributeType attTypePath = AttributeUtils.getAttributeTypeByName(sess, "パス");
			
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			
			selectTarget.setCondition(h.group(h.opAnd())
				//.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeFolder.getId()))
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name))
				.addCondition(h.eq(h.opAnd(), attTypePath, path))
				.addCondition(h.latest(h.opAnd())));
		}

		List<EIMObject> objList = SearchUtils.searchObjects(sess, selectTarget, null);
		return objList;
		
	}

	public EIMObject getLatestObjectByPathAndName(String path, String name) throws Exception {
		
		List<EIMObject> objList = this.getLatestObjectListByPathAndName(path, name);
		if(objList == null || objList.size() == 0) {
			return null;
		}
		if(objList.size() == 1){
			return objList.get(0);
		}else{
			EIMObject obj = null;
			for(EIMObject tmpObj:objList){
				if(tmpObj.getLatest()) obj = tmpObj;
			}
			return obj;
		}
	}
	
	@SuppressWarnings("unchecked")
	public List<EIMObject> getObjectListByPathAndName(String path, String name) throws Exception {
		
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		if("/".equals(path)) {
			
			EIMObjectType objTypeWorkSpace = ObjectUtils.getObjectTypeByName(sess, "ワークスペース");
			
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			
			selectTarget.setCondition(h.group(h.opAnd())
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeWorkSpace.getId()))
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name)));
		} else {
			
			//EIMObjectType objTypeFolder = ObjectUtils.getObjectTypeByName(sess, "フォルダ");
			EIMAttributeType attTypePath = AttributeUtils.getAttributeTypeByName(sess, "パス");
			
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			
			selectTarget.setCondition(h.group(h.opAnd())
				//.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeFolder.getId()))
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name))
				.addCondition(h.eq(h.opAnd(), attTypePath, path)));
		}

		List<EIMObject> objList = SearchUtils.searchObjects(sess, selectTarget, null);
		return objList;
		
	}
	
	@SuppressWarnings("unchecked")
	public List<EIMObject> getObjectListByPathAndNameForMkdirCommand(String path, String name) throws Exception {
		
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		if("/".equals(path)) {		
			EIMObjectType objTypeWorkSpace = ObjectUtils.getObjectTypeByName(sess, "ワークスペース");
			EIMObjectType objTrashbox = ObjectUtils.getObjectTypeByName(sess, "ごみ箱");
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			
			EIMSearchConditionGroup typeCondition = h.group(h.opAnd())
			.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeWorkSpace.getId()))
			.addCondition(h.eq(h.opOr(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTrashbox.getId()));
			
			EIMSearchConditionGroup nameCondition = h.group(h.opAnd()).addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name));
			
			selectTarget.setCondition(h.group(h.opAnd())
				.addCondition(typeCondition)
				.addCondition(nameCondition));
			
		} else {
			
			//EIMObjectType objTypeFolder = ObjectUtils.getObjectTypeByName(sess, "フォルダ");
			EIMAttributeType attTypePath = AttributeUtils.getAttributeTypeByName(sess, "パス");
			
			SearchConditionBuildHelper h = new SearchConditionBuildHelper();
			
			selectTarget.setCondition(h.group(h.opAnd())
				//.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, objTypeFolder.getId()))
				.addCondition(h.eq(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME, name))
				.addCondition(h.eq(h.opAnd(), attTypePath, path)));
		}

		List<EIMObject> objList = SearchUtils.searchObjects(sess, selectTarget, null);
		return objList;
		
	}
	
	public EIMObject getObjectByPathAndName(String path, String name) throws Exception {
		
		List<EIMObject> objList = this.getObjectListByPathAndName(path, name);
		if(objList == null || objList.size() == 0) {
			return null;
		}
		if(objList.size() == 1){
			return objList.get(0);
		}else{
			EIMObject obj = null;
			for(EIMObject tmpObj:objList){
				if(tmpObj.getLatest()) obj = tmpObj;
			}
			return obj;
		}
	}
	
	@SuppressWarnings("unchecked")
	public EIMObject createDocument(EIMObject parentObj, EIMObjectType objType, String name, File file) throws Exception {
		
		//Create Document Object
		EIMObject object = ObjectUtils.createObject(sess, objType, name);
		
		String path = "";
		if(parentObj.getAttribute("パス") != null) {
			//path = parentObj.getAttribute("パス").getString();
			path = parentObj.getAttribute("パス").getStrings()[0];
			
		} else {
			path = "/";
		}
		path += parentObj.getName() + "/";
		//ObjectAttributeUtils.setAttribute(sess, object, attTypePath, path);
		AppObjectUtil.setPath(sess, object, path);
		
		//Create User
		EIMAttributeType attTypeCreateUser = AttributeUtils.getAttributeTypeByName(sess, "作成者");
		ObjectAttributeUtils.setAttribute(sess, object, attTypeCreateUser, sess.getUser().getId());
		
		//Inherit Attributes From Parent Object
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		long[] parentLowAttrIds = AppObjectUtil.getIntAttrs(sess, parentObj, helper.getAttrNameOfToLowAttr());
		if(parentLowAttrIds != null) {
			List parentLowAttrTypes = new ArrayList();
			{
				List parentLowAttrIdsInteger = new ArrayList(Arrays.asList(ArrayUtils.toObject(parentLowAttrIds)));
				List objectTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
				SearchToLowAttr:for (Iterator i = parentLowAttrIdsInteger.iterator(); i.hasNext();) {
					long attrTypeIdOfParentLowAttrId = ((Long)i.next()).longValue();
					for(Iterator j = objectTypes.iterator(); j.hasNext();) {
						EIMAttributeType attrTypeObj = (EIMAttributeType)j.next();
						if(attrTypeObj.getId() == attrTypeIdOfParentLowAttrId) {
							parentLowAttrTypes.add(attrTypeObj);
							continue SearchToLowAttr;
						}
					}
					i.remove();
				}
				parentLowAttrIds = ArrayUtils.toPrimitive((Long[])parentLowAttrIdsInteger.toArray(new Long[parentLowAttrIdsInteger.size()]));
			}
			ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfFromHighAttr()
					, TypeConvertUtils.convertToBuildTypeArray(parentLowAttrIds));
			for(Iterator i = parentLowAttrTypes.iterator(); i.hasNext();) {
				EIMAttribute attr = parentObj.getAttribute(((EIMAttributeType)i.next()).getDefName());
				if(attr != null) {
					AppObjectUtil.setAttr(sess, object, attr);
				}
			}
			DisplayColorUtil.inheritDisplayColor(sess, object, parentLowAttrTypes, parentObj);
		}
		
		//Inherit Status From Parent Object
		if(parentObj.getStatus() != null) {
			WorkFlowUtils.updateObjectStatus(sess, object, parentObj.getStatus());
			EIMAttribute attrOfHigherWFFolder = parentObj.getAttribute(helper.getAttrNameDocumentHigherWFFolder());
			if(attrOfHigherWFFolder == null)
				ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfHigherWFFolder(), parentObj.getId());
			else
				AppObjectUtil.setAttr(sess, object, attrOfHigherWFFolder);
		}
		
		//Security
		EIMSecurity security = parentObj.getSecurity();
		if(security != null) {
			SecurityUtils.setSecurity(sess, object, security);
		}
		
		//Format
		EIMFormat format = FileUtils.getDefaultFormat(sess, objType);
		
		//Checkin
		FileUtils.checkin(sess, object, format, name, file.length());
		
		//Substance
		String serverPath = format.getDirectory().getPath() + object.getId();
		String fileExt = StringUtils.getFileExt(name);
		if(fileExt != null) {
			serverPath += fileExt;
		}
		
		//File Move does not work in NFS Volume, then Copy and Delete
		//file.renameTo(new File(serverPath));
		FileUtils.copyFile(file, new File(serverPath));
		file.delete();
		
		//When Workflow not Defined, then Publish
		if(object.getStatus() == null) {
			EIMFormat formatPublic = FileUtils.getFormatByName(sess, "公開ドキュメント");
			File orgFile = new File(serverPath);
			File dstFile = new File(formatPublic.getDirectory().getPath() + object.getId() + fileExt);
			FileUtils.copyFile(orgFile, dstFile);
			FileUtils.checkin(sess, object, formatPublic, name, file.length());
		}
		
		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.EXIF.INITIALREGIST");
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, "3001", "1", EIMConstant.OBJECT, object, null, null, null, EIMCommandService.VERSION + ":" + path + name);
		
		//Reload Object
		object = ObjectUtils.getObjectById(sess, object.getId());
		
		return object;
		
	}
	
	@SuppressWarnings("unchecked")
	public EIMObject createFolder(EIMObject parentObj, EIMObjectType objType, String name) throws Exception {
		
		//Check Invalid Char
		AppObjectUtil.checkValidateFName(sess, name);
		
		//Helper
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		//When Parent Object in Recycle Box
		if(AppObjectUtil.isObjectInRecycle(sess, parentObj)) {
			throw new Exception("Object Not Found");
		}
		
		//Check Parent Folder has Workflow
		if((helper.isTypeOfFolderWithWorkflow(parentObj) || helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj)) && WorkFlowUtils.getWorkFlowByType(sess, objType) != null) {
			throw new Exception("Cannot Create Workflow Folder under Workflow Folder");
		}
		
		//Check Create Role
		if(!SecurityUtils.enabled(sess, parentObj, sess.getUser(), EIMAccessRole.CREATE)) {
			throw new Exception("Not Allowed For Create");
		}
		
		//Check Parent Status because Necessary For Edit Status
		if(parentObj.getStatus() != null) {
			//if(parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_EDITTING) {
			if(parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				throw new Exception("Parent Status is Not Edit");
			}
		}
		
		//Check Under Folder Security
		long secId = AppObjectUtil.getIntAttr(sess, parentObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
		if(secId != Integer.MIN_VALUE) {
			if(!SecurityUtils.enabled(sess, parentObj, sess.getUser(), EIMAccessRole.UPDATE)) {
				throw new Exception("Under Folder Security not Allowed");
			}
		}
		
		//Path
		String path = AppObjectUtil.getPath(parentObj);
		if(path == null) {
			path = "/";
		}
		path += parentObj.getName() + "/";
		
		//Create Object
		EIMObject object = ObjectUtils.createObject(sess, objType, name);
		
		//Relation Type Document
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, "ドキュメント");
		
		//Create Relation
		RelationUtils.createRelation(sess, relType, parentObj, object, EIMConstant.DEPU_CHECK_NAME);
		
		//Set Path
		AppObjectUtil.setPath(sess, object, path);
		
		//Security
		if(parentObj.getSecurity() != null) {
			SecurityUtils.setSecurity(sess, object, parentObj.getSecurity());
		}
		
		//Check Parent Folder has Workflow
		if(helper.isTypeOfFolderWithWorkflow(parentObj)) {
			AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), parentObj.getId());
			WorkFlowUtils.updateObjectStatus(sess, object, parentObj.getStatus());
		} else {
			long higherWfFolderId = AppObjectUtil.getIntAttr(sess, parentObj, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), Integer.MIN_VALUE);
			if(higherWfFolderId != Integer.MIN_VALUE) {
				AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), higherWfFolderId);
				EIMObject higherWfFolder = ObjectUtils.getObjectById(sess, higherWfFolderId);
				WorkFlowUtils.updateObjectStatus(sess, object, higherWfFolder.getStatus());
			}
		}
		
		//Set Under Folder Security
		if(secId != Integer.MIN_VALUE) {
			AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), secId);
		}
		
		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.EXIF.INITIALREGIST");
		
		//Create Operation History
		OperationHistoryUtils.create(sess,AppConstant.DOCUMENT, EIMConstant.CREATE_FOLDER, EIMConstant.TARGET_CREATE, EIMConstant.OBJECT_TYPE, object, null, null, null, path);
		
		return object;
		
	}
	
	@SuppressWarnings("unchecked")
	public void deleteObject(EIMObject object) throws Exception {
		
		AppObjectConditionHelper appHelper = new AppObjectConditionHelper(sess);
		
		//Check Taged
		//if(AppLogicUtil.isTagAssignedObject(sess, object, sessHelper, appHelper, true)) {
		if(AppLogicUtil.isTagAssignedObject(sess, object, appHelper, true)) {
			throw new Exception("This Object is Taged");
		}
		
		//Check Delete Role
		if(object.getSecurity() != null && !SecurityUtils.enabled(sess, object, sess.getUser(), EIMAccessRole.DELETE)) {
			throw new Exception("Object Delete Not Allowed");
		}
		
		//Relation Type Document
		EIMRelationType relTypeDocument = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		
		//Relation Type Branch
		EIMRelationType relTypeBranch = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_BRANCH"));
		
		//Check Object Type
		if(isFolderTypes(object.getType())) {
			//AppLogicUtil.logicalDeleteFolderBranch(sess, object, relTypeBranch, relTypeDocument, appHelper, sessHelper);
			AppLogicUtil.logicalDeleteFolderBranch(sess, object, relTypeBranch, relTypeDocument, appHelper);
			//AppLogicUtil.physicalDeleteTagUnderFolder(sess, object, relTypeDocument, sessHelper, appHelper);
			AppLogicUtil.physicalDeleteTagUnderFolder(sess, object, relTypeDocument, appHelper);

			// SearchFramework 更新通知 対象：フォルダおよび配下のドキュメント、リンク、フォルダ
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_LOGICDEL_FOLDER");
			AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, object, 
					"SEARCHFW_LOGICDEL_CHILD_FOLDER", "SEARCHFW_LOGICDEL_CHILD_DOCUMENT", null, "SEARCHFW_LOGICDEL_CHILD_DOCUMENTLINK");

		} else if(isDocumentTypes(object.getType())) {
			// SearchFramework 更新通知 対象：ブランチ元ドキュメント
			//AppLogicUtil.logicalDeleteDocumentBranch(sess, object, relTypeBranch, sessHelper);
			AppLogicUtil.logicalDeleteDocumentBranch(sess, object, relTypeBranch);

			// SearchFramework 更新通知 対象：ドキュメント
			AppUpdateNoticeUtils.updateNoticeInsertObject(sess, object, "SEARCHFW_LOGICDEL_DOCUMENT", true);
		} else {
			throw new Exception("This Object Type cannot Delete" + "(" + object.getType().getName() + ")");
		}
		
		// SearchFramework 更新通知 対象：親フォルダor親ワークスペース
		if( AppUpdateNoticeUtils.doEntry() && 
				( AppUpdateNoticeUtils.isEnabledDataType("SEARCHFW_LOGICDEL_PARENT_FOLDER") 
				|| AppUpdateNoticeUtils.isEnabledDataType("SEARCHFW_LOGICDEL_PARENT_WORKSPACE")) )
		{
			List<EIMRelation> relList = RelationUtils.getParentRelationListByRelType(sess, object, relTypeDocument);
			for(EIMRelation rel: relList)
			{
				EIMObject parent = rel.getParent();
				AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parent, 
						"SEARCHFW_LOGICDEL_PARENT_FOLDER", "SEARCHFW_LOGICDEL_PARENT_WORKSPACE", null);
			}
		}
		
		//Recycle Object
		EIMObject recycleObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"), EIMConfig.get("OBJECT_TYPE_NAME_RECYCLE"));
		if(recycleObj == null) {
			throw new Exception("Recycle Object not Found");
		}
		
		//Get Path
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
		
		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.EXIF.DELETE");
		
		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.MOVE_TO_RECYCLEBOX, EIMConstant.TARGET_DELETE, EIMConstant.OBJECT, object, null, null, null, path);
		
		//Delete Relation
		List<EIMRelation> parentRelList = RelationUtils.getParentRelationListByRelType(sess, object, relTypeDocument);
		for(int i = 0; i < parentRelList.size(); i++) {
			RelationUtils.deleteRelation(sess, parentRelList.get(i));
		}
		
		//Create Relation to Recycle Object
		RelationUtils.createRelation(sess, relTypeDocument, recycleObj, object, EIMConstant.DEPU_CHECK_NONE);
		
		//Update Attribute For Delete
		//AttributeUtil.updateAttributeForDelete(sess, object, parentObj);
		ObjectAttributeUtils.setAttribute(sess, object, AttributeUtils.getAttributeTypeByName(sess, "パス"), new String[]{"/" + recycleObj.getName() + "/"});

		return;
		
	}
	
	@SuppressWarnings("unchecked")
	public List<EIMObject> getChildren(EIMObject parentObj) throws Exception {
		
		//Relation Type Document
		EIMRelationType relTypeDocument = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		
		//Get Children
		List<EIMRelation> childRelList = RelationUtils.getChildRelationListByRelType(sess, parentObj, relTypeDocument);
		List<EIMObject> childObjList = new ArrayList<EIMObject>();
		for(EIMRelation relation : childRelList) {
			childObjList.add(relation.getChild());
		}
		
		return childObjList;
		
	}
	
	@SuppressWarnings("unchecked")
	public boolean existsInChildrenByName(EIMObject parentObj, String name) throws Exception {
		List<EIMObject> childObjList = getChildren(parentObj);
		for(EIMObject object : childObjList) {
			if(object.getName().equals(name)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * オブジェクトタイプが「ワークスペース」であるEIMObjectのリストを取得する
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public List<EIMObject> getWorkSpaceList() throws Exception {
		
		//Object Type WorkSpace
		EIMObjectType objTypeWorkSpace = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"));
		
		//Get WorkSpace List
		List<EIMObject> objList = ObjectUtils.getObjectListByType(sess, objTypeWorkSpace);
		
		return objList;
		
	}
	
	@SuppressWarnings("unchecked")
	public boolean isDocumentTypes(EIMObjectType objType) throws Exception {
		
		objType = ObjectUtils.getObjectTypeById(sess, objType.getId());
		EIMObjectType rootObjType = objType;
		while(rootObjType.getParent() != null) {
			rootObjType = rootObjType.getParent();
		}
		
		if(rootObjType.getName().equals(OBJECT_TYPE_NAME_DOCUMENT)) {
			return true;
		} else {
			return false;
		}
		
	}
	
	@SuppressWarnings("unchecked")
	public boolean isFolderTypes(EIMObjectType objType) throws Exception {
		
		objType = ObjectUtils.getObjectTypeById(sess, objType.getId());
		EIMObjectType rootObjType = objType;
		while(rootObjType.getParent() != null) {
			rootObjType = rootObjType.getParent();
		}
		
		if(rootObjType.getName().equals(OBJECT_TYPE_NAME_WORKSPACE)) {
			return true;
		} else {
			return false;
		}
		
	}
	
	@SuppressWarnings("unchecked")
	public boolean isTagTypes(EIMObjectType objType) throws Exception {
		objType = ObjectUtils.getObjectTypeById(sess, objType.getId());
		EIMObjectType rootObjType = objType;
		while(rootObjType.getParent() != null) {
			rootObjType = rootObjType.getParent();
		}
		if(rootObjType.getName().equals(OBJECT_TYPE_NAME_TAG)) {
			return true;
		} else {
			return false;
		}
	}
	
	/**
	 * オブジェクトタイプ「タグ」を除いたEIMObjectのリストを返却する
	 * @param objList
	 * @return
	 */
	public List<EIMObject> getObjListExceptTag(List<EIMObject> objList) throws Exception
	{
		List<EIMObject> result = new ArrayList<EIMObject>();
		
		for(EIMObject obj : objList)
		{
			if(!isTagTypes(obj.getType()))
			{
				result.add(obj);
			}
		}
		
		return result;
	}
	
}
