<%@ page contentType="text/html; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.io.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>

<%@ page import = "common.bo.AttributeUpdater" %>
<%@ page import = "common.bo.AttributeUpdaterItem" %>

<%@page import="org.apache.commons.lang3.ArrayUtils"%>

<%
	Log log = LogFactory.getLog(this.getClass().getName());

	response.setContentType("text/html; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean sessPutFlag = false;

	Object[] paramId = null;

	//Message
	String message = null;

	try
	{
		//セッション情報の取得
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}

		loginUser = (EIMUser)sess.getAttribute("USER");


		// TransactionContextの作成、設定
		if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
		{
			jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		//クライアントから受け渡されたパラメタの取得
		MultiPartFormUtils dmpf = null;
		try{
			dmpf = new MultiPartFormUtils(request);
		}
		catch(EIMException eime) {
			sess.rollback();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
			out.println(AppMessageUtils.makeErrorTagByMessage(eime.getMessage()));
			return;
		}
		String prmObjId = dmpf.getParameter("objId");
		String prmObjTypeId = dmpf.getParameter("objTypeId");
		String prmCreateUserId = dmpf.getParameter("createUserId");
		String prmFileName = dmpf.getParameter("fileName");

		paramId = new Object[]{
				"objId=" + prmObjId,
				"objTypeId=" + prmObjTypeId,
				"createUserId=" + prmCreateUserId,
				"fileName=" + prmFileName
				};

		//オブジェクトタイプの取得
		EIMObjectType objType = null;
		//オブジェクトタイプチェック
		if (prmObjTypeId != null && ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId)) == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENTTYPE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		if(prmObjTypeId == null || prmObjTypeId.length() == 0)
		{
			objType = ObjectUtils.getObjectTypeByName(sess, "ドキュメント");
		}
		else
		{
			objType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmObjTypeId));
		}

		//親オブジェクトの取得
		EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(parentObj == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		//親オブジェクトがゴミ箱の下に移動していないかのチェック
		if (AppObjectUtil.isObjectInRecycle(sess, parentObj)) {
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// 作成権限のチェック
		if(!SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.CREATE))
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			sess.close();
			return;
		}
		//ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
		if (parentObj.getStatus() != null) {
			if (parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				sess.close();
				return;
			}
			if (WorkFlowUtils.getWorkFlowByType(sess, objType) != null) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				sess.close();
				return;
			}
		}

		//パスの取得
		String path = AppObjectUtil.getPath(parentObj);
		if(path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObj.getName() + "/";

		//フォーマット、ディレクトリの取得
		EIMFormat format = FileUtils.getDefaultFormat(sess, objType);
		if (format == null) {
			//デフォルトフォーマットが設定されていません。
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODEFAULTFORMAT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		EIMDirectory dir = format.getDirectory();

		//ファイル存在チェック
		if(dmpf.getFileSize() == 0)
		//if (mpf.getFileSize() == 0)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPLOADFILE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		//ファイル名、拡張子の取得
		String fileName = prmFileName.substring(prmFileName.lastIndexOf("/") + 1);
		String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(fileName));

		//指定されたドキュメントオブジェクトの作成
		EIMObject object = ObjectUtils.createObject(sess, objType, fileName);
		if(object == null)
		{
			sess.rollback();
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILMAKEOBJ");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		//親オブジェクトとのリレーションを作成
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, "ドキュメント");
		try{
			RelationUtils.createRelation(sess, relType, parentObj, object,  EIMConstant.DEPU_CHECK_NAME_REV);
		}
		catch(EIMException eime) {
			sess.rollback();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
			out.println(AppMessageUtils.makeErrorTagByMessage(eime.getMessage()));
			return;
		}

		//パスを設定
		AppObjectUtil.setPath(sess, object, path);

		EIMAttributeType attType = null;

		//作成者を設定
		if(prmCreateUserId != null)
		{
			EIMUser createUser = UserUtils.getUserById(sess, Long.parseLong(prmCreateUserId));

			attType = AttributeUtils.getAttributeTypeByName(sess, "作成者");
			ObjectAttributeUtils.setAttribute(sess, object, attType, createUser.getId());
		}

		// 連続データIDの取得
		String nextValue = AppObjectTypeUtil.getNextValue(objType.getId());
		if (nextValue != null) {
			AppObjectUtil.setAttrForce(sess, object, "番号", nextValue);
		}

		//上位フォルダからの属性引継ぎ
		//(クライアントから送られる情報は入力可能な属性情報しか送られない為、別途引継属性を設定する。（既存処理）)
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		long[] parentLowAttrIds = AppObjectUtil.getIntAttrs(sess, parentObj, helper.getAttrNameOfToLowAttr());

		if (parentLowAttrIds != null) {
			//上位からの引継ぎ属性の設定内容に従い、parentObjから属性値をコピー
			//ただし、自身のタイプに該当属性が割り当てられているものに限る
			List parentLowAttrTypes = new ArrayList();
			{
				List parentLowAttrIdsInteger = new ArrayList(Arrays.asList(ArrayUtils.toObject(parentLowAttrIds)));
				List objectTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
				//引継ぎ対象を自身のタイプに該当属性が割り当てられているものにフィルタリング
SearchToLowAttr:for (Iterator i = parentLowAttrIdsInteger.iterator(); i.hasNext();) {
					long attrTypeIdOfParentLowAttrId = ((Long)i.next()).longValue();
					for (Iterator j = objectTypes.iterator(); j.hasNext();) {
						EIMAttributeType attrTypeObj = (EIMAttributeType)j.next();
						if (attrTypeObj.getId() == attrTypeIdOfParentLowAttrId) {
							parentLowAttrTypes.add(attrTypeObj);
							continue SearchToLowAttr;
						}
					}
					i.remove();//自身のタイプに無かったので対象から削除
				}
				parentLowAttrIds = ArrayUtils.toPrimitive((Long[])parentLowAttrIdsInteger.toArray(new Long[parentLowAttrIdsInteger.size()]));
			}
			//「上位からの引継ぎ」属性の値を設定
			ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfFromHighAttr(), TypeConvertUtils.convertToBuildTypeArray(parentLowAttrIds));

			//各属性値の引継ぎ
			for (Iterator i = parentLowAttrTypes.iterator(); i.hasNext();) {
				EIMAttribute attr = parentObj.getAttribute(((EIMAttributeType)i.next()).getDefName());
				if (attr != null) {
					AppObjectUtil.setAttr(sess, object, attr);
				}
			}

			//リスト値表示色オブジェクトの引継ぎ
			DisplayColorUtil.inheritDisplayColor(sess, object, parentLowAttrTypes, parentObj);
		}

		//上位フォルダからのステータス引継ぎ
		if (parentObj.getStatus() != null) {
			WorkFlowUtils.updateObjectStatus(sess, object, parentObj.getStatus());

			//「上位WFフォルダ」属性も登録
			EIMAttribute attrOfHigherWFFolder = parentObj.getAttribute(helper.getAttrNameDocumentHigherWFFolder());
			if (attrOfHigherWFFolder == null) //WF付フォルダ直下
				ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfHigherWFFolder(), parentObj.getId());
			else //「WF付フォルダ下のフォルダ」の下
				AppObjectUtil.setAttr(sess, object, attrOfHigherWFFolder);
		}

		//セキュリティを設定
		EIMSecurity sec = parentObj.getSecurity();
		if(sec != null)
		{
			SecurityUtils.setSecurity(sess, object, sec);
		}

		//チェックイン実行（DBに登録）
		FileUtils.checkin(sess, object, format,fileName,dmpf.getFileSize());//
		//取得した実ファイルをファイルサーバ上に配置する
		dmpf.upload(dir.getPath() +object.getId()+ fileExt);
		//ちなみに実データを削除する際にはこの実データのファイル名は参照しないので、実データのファイル名は何でもよい

		// 作成したドキュメント自身にステータスが無く、かつ上位フォルダにもステータスが無い場合は、
		// WFなしドキュメントとして、即公開する
		if (object.getStatus() == null && parentObj.getStatus() == null) {

			// 公開ドキュメントとして登録
			EIMFormat publicDocumentFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
			File orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(object, file));
			File dstFile = new File(publicDocumentFormat.getDirectory().getPath() + object.getId() + file.getExt());
			FileUtils.createSymbolicLink(orgFile, dstFile);
			FileUtils.checkin(sess, object, publicDocumentFormat, file.getName(), file.getSize());
		}

		// OCR
		if(OptionConfData.getInstance().ocrFlg){
			if(AppOcrUtil.hasOcrSetting(sess, object)){
				// OCR設定有無の有りの場合、OCR処理ステータスに0:処理待を設定する
				AppOcrUtil.setOcrProcessingStatus(sess, object, AppConstant.OCR_PROC_STATUS_PROCESS_WAIT);
			}
		}

		//Access
		AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.INITIALREGIST");

		// SearchFramework 検索FW更新通知 対象：ドキュメント + 親フォルダ・親ワークスペース
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CREATE_DOCUMENT");
		AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_CREATE_DOCUMENT_PARENT_FOLDER", "SEARCHFW_CREATE_DOCUMENT_PARENT_WORKSPACE", null);

		//Create Operation History
		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.CREATE_DOCUMENT,
				EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, object,
				null, null, null, path);

		//XML
		out.println("<object");

			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");

		out.println(">");
		out.println("</object>");

		//Commit
		sess.commit();
	}
	catch(EIMException eime)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
	}
	finally
	{
		try{
			if(sessPutFlag) {
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
			}

			if(sess != null){
				sess.close();
			}

			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
			}

		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
		}
	}
%>
