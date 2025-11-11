<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "java.io.File" %>
<%@ page import = "java.util.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.app.document.business.domain.DocumentAttributeTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.AttributeTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.domain.ObjectTypeLayoutDomain"%>
<%@ page import = "jp.co.ctc_g.eim.app.form.business.service.ObjectTypeLayoutService"%>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "org.apache.commons.logging.*" %>
<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean sessPutFlag = false;

	//Message
	String message = null;
	Object paramId = null;

	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		loginUser = (EIMUser)sess.getAttribute("USER");

		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}
		// TransactionContextの作成、設定
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		//String prmObjId = request.getParameter("objId"+0);
		Map<String, Long> objectIdMap = new HashMap<String, Long>();
		List<Long> objectIdList = new ArrayList<Long>();
		String prmObjId = request.getParameter("objId"+0);
		Map<Long, String> requestParamIndexMap = new HashMap<Long, String>();
		for (int ii = 0; prmObjId != null; ii++, prmObjId = request.getParameter("objId"+ii)) {
			objectIdList.add(Long.parseLong(prmObjId));
			requestParamIndexMap.put(Long.parseLong(prmObjId), "objId"+ii);
		}

		String prmIsReturnRevUpObjectId = request.getParameter("isReturnRevUpObjectId");
		boolean isReturnRevUpObjectId = false;
		if (prmIsReturnRevUpObjectId != null && prmIsReturnRevUpObjectId.equals("true")) {
			isReturnRevUpObjectId = true;
		}

		//先に処理対象ドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
		List<Long> lockObjIdList = new ArrayList<Long>();
		for(int i = 0; i < objectIdList.size() ; i ++){
			long objId = objectIdList.get(i);
			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, objId);
			if(object == null)
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, new Object[]{objId}));
				return;
			}
			// 排他制御用に行ロックをかけるオブジェクト全てのIDをリストに格納する
			List<EIMObject> objListInVersion = VersionUtils.getVersion(sess, object).getList();
			for(EIMObject lockObj : objListInVersion)
			{
				lockObjIdList.add((long)lockObj.getId());
			}
			objectIdMap.put("objId" + i, (long)object.getId());
		}
		long[] objIds = new long[lockObjIdList.size()];
		for(int i = 0; i < lockObjIdList.size(); i++)
		{
			objIds[i] = lockObjIdList.get(i);
		}
		AppObjectUtil.lockObjectById(sess, objIds);

		out.println("<objectList>");
		prmObjId = request.getParameter("objId"+0);
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		for (int ii = 0; prmObjId != null; ii++, prmObjId = request.getParameter("objId"+ii)) {

			//paramId = "objId" + ii + "=" + prmObjId;

			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, objectIdMap.get("objId"+ii));
			paramId = requestParamIndexMap.get((long)object.getId()) + "=" + object.getId();
			if(object == null)
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, new Object[]{objectIdMap.get("objId"+ii)}));
				return;
			}

			//権限チェック
			boolean enabled = true;
			do {
				//Check REVISION_UP Role
				if(object.getSecurity() != null
						&& !SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.REVISION_UP)) {
						enabled = false;
						break;
				}
				//ワークフロー付きフォルダ下の場合は、チェックアウトできない
				if (helper.isUnderFolderWithWorkflow(object)) {
					enabled = false;
					break;
				}
			} while (false);
			if(!enabled)
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCHECKOUTROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKOUTROLE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, new Object[]{paramId}));
				return;
			}

			//Check Status (WFなしドキュメントもチェックアウトできる)
			if(object.getStatus() != null && object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{StringUtils.xmlEncode(object.getName())});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTPUBLIC", new Object[]{StringUtils.xmlEncode(object.getName())});
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, new Object[]{paramId}));
				return;
			}

			//Check Lock User
			EIMUser user = object.getLockUser();
			if ((user != null)
					|| (object.getStatus() == null // WFなしドキュメントの場合はチェックアウト可否判定をする
							&& !AppLogicUtil.isCheckoutEnabledNoWFDoc(sess, object)))
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CHECKOUTED", new Object[]{StringUtils.xmlEncode(object.getName())});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.CHECKOUTED", new Object[]{StringUtils.xmlEncode(object.getName())});
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, new Object[]{paramId}));
				return;
			}

			//対象が最新のリビジョンかチェック
			EIMVersion version = VersionUtils.getVersion(sess, object);
			if(object.getRevision() != version.getMaxRevision())
			{
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCHECKOUTROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKOUTROLE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, new Object[]{paramId}));
				return;
			}

			//Lock
			EIMObject newObject = VersionUtils.revisionUp(sess, object);

			// オブジェクト（関連ドキュメント、添付ドキュメント）をリビジョンアップ元から引き継ぐ

			// カスタム属性(レイアウト)含むオブジェクトタイプを取得
			ObjectTypeLayoutService objectTypeLayoutService =
				(ObjectTypeLayoutService)ApplicationContextLoader.getApplicationContext().getBean("documentObjectTypeLayoutService");
			ObjectTypeLayoutDomain objTypeLayoutdomain =
				objectTypeLayoutService.getById(new Long(object.getType().getId()));

			// 引継ぎ対象の属性タイプID
			Set<String> inheritanceAttributeTypeIdSet = new HashSet<String>();
			for (AttributeTypeLayoutDomain atttypeLayout : objTypeLayoutdomain.getAttributeLayoutList()) {
				if (!((DocumentAttributeTypeLayoutDomain)atttypeLayout).isInheritanceFlag()) {
					continue;
				}
				inheritanceAttributeTypeIdSet.add(String.valueOf(atttypeLayout.getId()));

			}

			// リビジョンアップ対象（引継ぎ元、先オブジェクト）取得
			ObjectService objectService =
					(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("documentObjectService2");
			ObjectDomain orgObjectDomain = objectService.getById(object.getId());
			ObjectDomain newObjectDomain = objectService.getById(newObject.getId());

			// 各属性タイプの内、引継ぎ対象を引継ぎ先に設定する
			List<AttributeDomain> attList = orgObjectDomain.getAttributeList();
			for (int i = 0; i < attList.size(); i++) {
				// Attribute
				AttributeDomain att = attList.get(i);

				// Attribute Type
				AttributeTypeDomain attType = att.getAttributeType();

				// 属性タイプのデータ型がObjectでない場合はコピー対象外
				if (attType.getValueType() != ValueTypeEnum.OBJECT &&
						attType.getValueType() != ValueTypeEnum.USER) {
					continue;
				}

				// 属性に設定されたオブジェクトのタイプをチェック
				if (attType.getValueType() == ValueTypeEnum.OBJECT) {

					// オブジェクト型の場合

					// 引き継ぐ属性内のオブジェクトリスト
					List<ObjectDomain> attributeObjects = new ArrayList<ObjectDomain>();

					// 引継ぎ対象かチェック
					if (!inheritanceAttributeTypeIdSet.contains(String.valueOf(attType.getId()))) {
						continue;
					}

					if (att.getObjectList().get(0).getType().getDefinitionName().equals(
							EIMConfig.getValue("OBJECT_TYPE_NAME_FORM_ATTACH_FILE"))) {

						// 添付ファイルの場合

						// 添付ファイルをコピーして引き継ぐ
						int attSize = att.getObjectList().size();
						for (int j = 0; j < attSize; j++) {
							ObjectDomain orgAttachmentObject = att.getObjectList().get(j);
							ObjectDomain newAttachementObject = new ObjectDomain();

							newAttachementObject.setName(orgAttachmentObject.getName());
							newAttachementObject.setType(orgAttachmentObject.getType());

							// 添付ファイル属性を引き継ぐ
							ObjectService attachementObjectService =
									(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("attachementObjectService");
							newAttachementObject = attachementObjectService.copy(orgAttachmentObject, newAttachementObject);
							attributeObjects.add(newAttachementObject);
						}
					}
					else {
						// 関連ドキュメントの場合

						// 関連ドキュメントのIDを引き継ぐ
						attributeObjects = att.getObjectList();
					}

					if (attType.isMultiple()) {
						objectService.setAttributeMultipleObject(newObjectDomain, attType, attributeObjects);
					} else {
						objectService.setAttributeSingleObject(newObjectDomain, attType, attributeObjects.get(0));
					}
				}
				else {
					// ユーザ型の場合

					// 関連元のユーザ型属性をそのまま引き継ぐ
					if (attType.isMultiple()) {
						objectService.setAttributeMultipleUser(newObjectDomain, attType, att.getUserList());
					} else {
						objectService.setAttributeSingleUser(newObjectDomain, attType, att.getUserList().get(0));
					}
				}

			}

			// WFありの場合、公開フォーマットのメタ情報と実ファイルを削除
			if (helper.isTypeOfDocumentWithWorkflow(newObject)) {
				EIMFormat newObjPubFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
				EIMFile newObjPubMetaInfo = FileUtils.getFile(sess, newObject, newObjPubFormat);
				FileUtils.deleteFile(sess, newObject, newObjPubFormat);

				File newObjPubFile = new File(newObjPubMetaInfo.getDirectory().getPath() + FileUtils.getFileName(newObject, newObjPubMetaInfo));
				if (newObjPubFile != null) {
					newObjPubFile.delete();
				}
			}else{
				// WFなしの場合、公開フォーマットの実ファイルを削除し、シンボリックリンクを作成。
				EIMFormat newObjPubFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
				EIMFile baseFile = FileUtils.getFile(sess, newObject, FileUtils.getDefaultFormat(sess, newObject.getType()));

				File orgFile = new File(baseFile.getDirectory().getPath() + FileUtils.getFileName(newObject, baseFile));
				File newObjPubFile = new File(newObjPubFormat.getDirectory().getPath() + newObject.getId() + baseFile.getExt());
				FileUtils.createSymbolicLink(orgFile, newObjPubFile);
			}

			// 継承しない属性を削除
			AttributeUtil.deleteNonInheritAttribute(sess, newObject);


			// 属性表示色を更新（オブジェクトをもう一度取り直す）
			newObject = ObjectUtils.getObjectById(sess, newObject.getId());
			DisplayColorUtil.copyDisplayColor(sess, newObject, object);

			// OCR
			if(OptionConfData.getInstance().ocrFlg){
				if(AppOcrUtil.hasOcrSetting(sess, newObject)){
					// OCR設定有無の有りの場合、OCR処理ステータスに0:処理待を設定する
					AppOcrUtil.setOcrProcessingStatus(sess, newObject, AppConstant.OCR_PROC_STATUS_PROCESS_WAIT);
				}
			}

			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

			// パス属性は先頭の一つのみをセットする(複数値の2つ目以降は継承元ドキュメントのリンク情報のため)
			AppObjectUtil.setPath(sess, newObject, path);

			object = VersionUtils.setLatest(sess, object);
			//ドキュメント管理の仕様でlatestflagをobjectとnewObjectで２つ立てる
			// VersionUtils.setLatest(sess, object);ではフラグを１つしか立てないようにしているので、
			//かならずこの後に呼ぶ
			VersionUtils.setLatestWithNoCheck(sess, newObject, true);

			//Access
			AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.LOCK");
			AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.REBISIONUP");

			// SearchFramework 検索FW更新通知 対象：新ドキュメント + 旧ドキュメント
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CHECK_OUT_DOCUMENT");
			AppUpdateNoticeUtils.updateNoticeInsert(newObject.getId(), "SEARCHFW_CHECK_OUT_NEW_DOCUMENT");


			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.CHECKOUT,
					EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
					null, null, null, path);

			//XML
			out.println("<object");
				out.println(" objId=\"" + object.getId() + "\"");
			if (isReturnRevUpObjectId) {
				out.println(" revUpObjId=\"" + newObject.getId() + "\"");
			}
			out.println(">");
			out.println("</object>");
		}
		out.println("</objectList>");

		//Commit
		sess.commit();
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), new Object[]{paramId}), eime);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		try{
			if(sess != null){
				sess.rollback();
			}
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
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
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}



%>
