package app.document;

import java.io.File;
import java.util.TimeZone;

import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eimtest.app.util.TestAppObjectUtil;
import eimtest.app.util.net.EIMHttpServiceCaller;
import eimtest.util.Misc;

/** */
public class DocumentService extends EIMHttpServiceCaller {
	/**
	 * 
	 * @throws Exception
	 */
	public DocumentService() throws Exception {
		this("system", "manager");
	}

	/**
	 * 
	 * @param userCode
	 * @param userPass
	 * @throws Exception
	 */
	public DocumentService(String userCode, String userPass) throws Exception {
		this(userCode, userPass, null, null);
	}

	/**
	 * 
	 * @param userCode
	 * @param userPass
	 * @param lang
	 * @param tz
	 * @throws Exception
	 */
	public DocumentService(String userCode, String userPass, String lang, TimeZone tz)
			throws Exception {
		super("app/document");
		br.eimLogin(userCode, userPass, lang, tz);
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public String approve_dspApproveDocumentList() throws Exception {
		return get("approve/dspApproveDocumentList.jsp", new Object[][] {//
			});

	}

	/**
	 * 
	 * @param objId
	 * @param approverId
	 * @param publisherIds
	 * @param comment
	 * @param delayMail
	 * @param doReply
	 * @return o
	 * @throws Exception
	 */
	public String approve_actRequestApprove(int objId, String approverId, String[] publisherIds,
			String comment, boolean delayMail, boolean doReply) throws Exception {
		return get("approve/actRequestApprove.jsp", new Object[][] {//
			{ "objId", String.valueOf(objId) }//
					, { "approverId", approverId }//
					, { "publisherId", Misc.toCsv(publisherIds) }//
					, { "comment", comment }//
					, { "timing", delayMail ? "1" : "0" }//
					, { "reply", doReply ? "1" : "0" } //
			});
	}

	/**
	 * 
	 * @param obj
	 * @param functionType
	 * @param approverId
	 * @param comment
	 * @param finalApprove
	 * @param mailNow
	 * @return o
	 * @throws Exception
	 */
	public String approve_actApprove(EIMObject obj, String functionType, int approverId,
			String comment, boolean finalApprove, boolean mailNow) throws Exception {
		return get("approve/actApprove.jsp", new Object[][] {//
			{ "objId", String.valueOf(obj.getId()) }//
					, { "functionType", functionType }//
					, { "approverId", String.valueOf(approverId) }// 
					, { "comment", comment }//
					, { "finalApprove", String.valueOf(finalApprove) }//
					, { "timing", mailNow ? "1" : "0" }//
					, { "statusId", String.valueOf(obj.getStatus().getId()) } //
			});
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String approve_actCancelRequestApprove(int objId) throws Exception {
		return get("approve/actCancelRequestApprove.jsp", new Object[][] {//
			{ "objId", String.valueOf(objId) } //
			});
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String approve_dspProperty(int objId) throws Exception {
		return get("approve/dspProperty.jsp", new Object[][] {//
			{ "objId", String.valueOf(objId) } //		
			});
	}

	/**
	 * 
	 * @param folderName
	 * @param attrTreeId
	 * @param attrTreeValues
	 * @param attrTreeSettings
	 * @param attrTreePath
	 * @return o
	 * @throws Exception
	 */
	public String folder_dspChildObject(String folderName, int attrTreeId, String[] attrTreeValues,
			String attrTreeSettings, String attrTreePath) throws Exception {
		EIMObject folder = (folderName != null) ? TestAppObjectUtil.getFolderObject(folderName)
				: null;
		Object[][] params = new Object[//
		((folderName != null) ? 1 : 0)//
				+ ((attrTreeId > 0) ? 1 : 0)//
				+ ((attrTreeValues == null) ? 0 : attrTreeValues.length)//
				+ ((attrTreeSettings == null) ? 0 : 1)//
				+ ((attrTreePath == null) ? 0 : 1)][];
		int p = 0;
		if (folderName != null)
			params[p++] = new Object[] { "objId", String.valueOf(folder.getId()) };
		if (attrTreeId > 0)
			params[p++] = new Object[] { "attrTreeId", String.valueOf(attrTreeId) };

		if (attrTreeValues != null)
			for (int i = 0; i < attrTreeValues.length; i++) {
				params[p++] = new Object[] { "attrTreeValue" + (i + 1), attrTreeValues[i] };
			}
		if (attrTreeSettings != null)
			params[p++] = new Object[] { "attrTreeSettings", attrTreeSettings };
		if (attrTreePath != null)
			params[p++] = new Object[] { "attrTreePath", attrTreePath };

		return get("folder/dspChildObject.jsp", params);
	}

	/**
	 * 
	 * @param folderName
	 * @param docType
	 * @param property
	 * @param createUserId
	 * @param expireDate
	 * @param uploadFile
	 * @return o
	 * @throws Exception
	 */
	public String object_actCreateDocument(String folderName, EIMObjectType docType,
			String property, int createUserId, String expireDate, File uploadFile) throws Exception {
		EIMObject folder = TestAppObjectUtil.getFolderObject(folderName);
		String res = get("object/actCreateDocument.jsp", new Object[][] {//
			{ "objId", String.valueOf(folder.getId()) }//		
					, { "objTypeId", String.valueOf(docType.getId()) } //	
					, { "property", property }//
					, { "createUserId", String.valueOf(createUserId) }//
					, { "expireDate", expireDate }//
					, { "fileName", uploadFile.getName() } //
					, { "Filedata", uploadFile } //
			});
		return res;
	}

	/**
	 * 
	 * @param obj
	 * @return o
	 * @throws Exception
	 */
	public String object_actCheckout(EIMObject obj) throws Exception {
		return get("object/actCheckout.jsp", new Object[][] {//
			{ "objId0", String.valueOf(obj.getId()) } //		
			});
	}

	/**
	 * 
	 * @param obj
	 * @return o
	 * @throws Exception
	 */
	public String object_dspCheckin(EIMObject obj) throws Exception {
		return get("object/dspCheckin.jsp", new Object[][] {//
			{ "objId", String.valueOf(obj.getId()) } //		
			});
	}

	/**
	 * 
	 * @param objId
	 * @param objTypeId
	 * @return o
	 * @throws Exception
	 */
	public String object_dspCreateOneDocument(int objId, int objTypeId) throws Exception {
		Object[][] params = new Object[1 + ((objTypeId > 0) ? 1 : 0)][];
		params[0] = new Object[] { "objId", String.valueOf(objId) };
		if (objTypeId > 0)
			params[1] = new Object[] { "objTypeId", String.valueOf(objTypeId) };
		return get("object/dspCreateOneDocument.jsp", params);
	}

	/**
	 * 
	 * @param objId
	 * @param objTypeId
	 * @param property
	 * @param createUserId
	 * @param expireDate
	 * @param fileName
	 * @param file
	 * @return o
	 * @throws Exception
	 */
	public String object_actCreateDocument(int objId, int objTypeId, String property,
			int createUserId, String expireDate, String fileName, File file) throws Exception {
		Object[][] params = new Object[6 + ((objTypeId > 0) ? 1 : 0)][];
		int p = 0;
		params[p++] = new Object[] { "objId", String.valueOf(objId) };
		if (objTypeId > 0)
			params[p++] = new Object[] { "objTypeId", String.valueOf(objTypeId) };
		params[p++] = new Object[] { "property", property };//
		params[p++] = new Object[] { "createUserId", String.valueOf(createUserId) };
		params[p++] = new Object[] { "expireDate", expireDate };
		params[p++] = new Object[] { "fileName", fileName };
		params[p++] = new Object[] { "Filedata", file };

		return get("object/actCreateDocument.jsp", params);
	}

	/**
	 * 
	 * @param objId
	 * @param filename
	 * @param file
	 * @param comment
	 * @return o
	 * @throws Exception
	 */
	public String object_actCheckin(int objId, String filename, File file, String comment)
			throws Exception {
		return get("object/actCheckin.jsp", new Object[][] {//
			{ "objId", String.valueOf(objId) } //
					, { "fileName", filename }//
					, { "updateComment", comment }//
					, { "Filedata", file } });
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String object_dspAttribute(int objId) throws Exception {
		return get("object/dspAttribute.jsp", new Object[][] {//
			{ "objId", String.valueOf(objId) } //		
			});
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String object_dspProperty(int objId) throws Exception {
		return get("object/dspProperty.jsp", new Object[][] {//
			{ "objId", String.valueOf(objId) } //		
			});
	}

	/**
	 * 
	 * @param objId
	 * @param parentObjId
	 * @return o
	 * @throws Exception
	 */
	public String object_actDeleteObject(int objId, int parentObjId) throws Exception {
		return get("object/actDeleteObject.jsp", new Object[][] {//
			{ "objId", String.valueOf(objId) }//
					, { "isFolder", "this parameter is not use now" }//
					, { "parentObjId", String.valueOf(parentObjId) } });
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String user_actSendReplyMail(int objId) throws Exception {
		return get("user/actSendReplyMail.jsp", new Object[][] {//
			{ "objId", String.valueOf(objId) } //		
			});
	}

	/**
	 * 
	 * @param attrTypeName
	 * @return o
	 * @throws Exception
	 */
	public String table_dspAttributeTypeList(String attrTypeName) throws Exception {
		return get("table/dspAttributeTypeList.jsp", new Object[][] {//
			{ "attTypeName", attrTypeName } //		
			});
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public String session_dspSession() throws Exception {
		return get("session/dspSession.jsp", new Object[][] {//
			});
	}

	/**
	 * 
	 * @param attrTreeId -1の時はattrTreeIdパラメータを送信しません
	 * @param attrTreeValues
	 * @param attrTreeSettings
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_dspAttrTree(int attrTreeId, String[] attrTreeValues,
			String attrTreeSettings) throws Exception {
		Object[][] params = new Object[//
		((attrTreeId > 0) ? 1 : 0)//
				+ ((attrTreeValues == null) ? 0 : attrTreeValues.length)//
				+ ((attrTreeSettings == null) ? 0 : 1)][];
		int p = 0;
		if (attrTreeId > 0)
			params[p++] = new Object[] { "attrTreeId", String.valueOf(attrTreeId) };

		if (attrTreeValues != null)
			for (int i = 0; i < attrTreeValues.length; i++) {
				params[p++] = new Object[] { "attrTreeValue" + (i + 1), attrTreeValues[i] };
			}
		if (attrTreeSettings != null)
			params[p++] = new Object[] { "attrTreeSettings", attrTreeSettings };

		return get("attrTree/dspAttrTree.jsp", params);
	}

	/**
	 * 
	 * @param attrTreeId -1の時はattrTreeIdパラメータを送信しません
	 * @param attrTreeValues
	 * @param attrTreeSettings
	 * @return o
	 * @throws Exception
	 */
	public String attrTree_dspAttrTreeChild(int attrTreeId, String[] attrTreeValues,
			String attrTreeSettings) throws Exception {
		Object[][] params = new Object[//
		((attrTreeId > 0) ? 1 : 0)//
				+ ((attrTreeValues == null) ? 0 : attrTreeValues.length)//
				+ ((attrTreeSettings == null) ? 0 : 1)][];
		int p = 0;
		if (attrTreeId > 0)
			params[p++] = new Object[] { "attrTreeId", String.valueOf(attrTreeId) };

		if (attrTreeValues != null)
			for (int i = 0; i < attrTreeValues.length; i++) {
				params[p++] = new Object[] { "attrTreeValue" + (i + 1), attrTreeValues[i] };
			}
		if (attrTreeSettings != null)
			params[p++] = new Object[] { "attrTreeSettings", attrTreeSettings };

		return get("attrTree/dspAttrTreeChild.jsp", params);
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public String folder_dspFixedFormFolderList() throws Exception {
		return get("folder/dspFixedFormFolderList.jsp", null);
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public String folder_dspFolderTree() throws Exception {
		return get("folder/dspFolderTree.jsp", null);
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String folder_dspFolderTreeForTarget(int objId) throws Exception {
		return get("folder/dspFolderTreeForTarget.jsp", new Object[][] { { "ObjectId",
				String.valueOf(objId) } });
	}

	/**
	 * 
	 * @param objId
	 * @param attrTreeId
	 * @param attrTreeValues
	 * @param attrTreeSettings
	 * @return o
	 * @throws Exception
	 */
	public String folder_dspFolderTreeChild(int objId, int attrTreeId, String[] attrTreeValues,
			String attrTreeSettings) throws Exception {
		Object[][] params = new Object[//
		1//
				+ ((attrTreeId > 0) ? 1 : 0)//
				+ ((attrTreeValues == null) ? 0 : attrTreeValues.length)//
				+ ((attrTreeSettings == null) ? 0 : 1)][];
		int p = 0;
		params[p++] = new Object[] { "ObjectId", String.valueOf(objId) };
		if (attrTreeId > 0)
			params[p++] = new Object[] { "attrTreeId", String.valueOf(attrTreeId) };

		if (attrTreeValues != null)
			for (int i = 0; i < attrTreeValues.length; i++) {
				params[p++] = new Object[] { "attrTreeValue" + (i + 1), attrTreeValues[i] };
			}
		if (attrTreeSettings != null)
			params[p++] = new Object[] { "attrTreeSettings", attrTreeSettings };
		return get("folder/dspFolderTreeChild.jsp", params);
	}

	/**
	 * 
	 * @param objId
	 * @param isURLLogin
	 * @return o
	 * @throws Exception
	 */
	public String object_dspParentFolder(int objId, String isURLLogin) throws Exception {
		Object[][] params = new Object[//
		1//
		+ ((isURLLogin == null) ? 0 : 1)//
		][];
		int p = 0;
		params[p++] = new Object[] { "objId", String.valueOf(objId) };
		if (isURLLogin != null)
			params[p++] = new Object[] { "isURLLogin", isURLLogin };
		return get("object/dspParentFolder.jsp", params);
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String user_actCreateFavorite(int objId) throws Exception {
		return get("user/actCreateFavorite.jsp",
			new Object[][] { { "objId", String.valueOf(objId) } });
	}

	/**
	 * 
	 * @return o
	 * @throws Exception
	 */
	public String user_dspFavoriteList() throws Exception {
		return get("user/dspFavoriteList.jsp", null);
	}

	/**
	 * 
	 * @param folderId
	 * @return o
	 * @throws Exception
	 */
	public String folder_actDuplicateFolderTree(int folderId) throws Exception {
		return get("folder/actDuplicateFolderTree.jsp", new Object[][] { { "folderId",
				String.valueOf(folderId) } });
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String servlet_dl_DownloadPrivateDocument(int objId) throws Exception {
		return get("/servlet/DownloadPrivateDocument", new Object[][] { { "objId",
				String.valueOf(objId) } });
	}

	/**
	 * 
	 * @param objId
	 * @return o
	 * @throws Exception
	 */
	public String servlet_dl_DownloadPublicDocument(int objId) throws Exception {
		return get("/servlet/DownloadPublicDocument", new Object[][] { { "objId",
				String.valueOf(objId) } });
	}
}
