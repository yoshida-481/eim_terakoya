<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.io.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	class BelongingWSUtils
	{
		//コンストラクタ
		public BelongingWSUtils()
		{
		}

		//親の所属ワークスペースを引き継ぐ
		public void SetBelongingWS(EIMSession sess, EIMObject newObj, EIMObject parentObj) throws Exception
		{
			try {
				EIMAttributeType typeWS = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_BELONGING_WS"));
				if(parentObj.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"))){
					ObjectAttributeUtils.setAttribute(sess, newObj, typeWS, parentObj.getId());
				}else{
					EIMAttribute parentWS = parentObj.getAttribute(typeWS.getDefName());
					ObjectAttributeUtils.setAttribute(sess, newObj, typeWS, parentWS.getInt());
				}
			} catch (Exception e) {
				// エラー時は何もしない
			}
			try {
				EIMAttributeType typeLWS =AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_LINK_BELONGING_WS"));
				if(parentObj.getType().getDefName().equals(EIMConfig.get("OBJECT_TYPE_NAME_WORKSPACE"))){
					ObjectAttributeUtils.setAttribute(sess, newObj, typeLWS, parentObj.getId());
				}else{
					EIMAttribute parentLWS = parentObj.getAttribute(typeLWS.getDefName());
					ObjectAttributeUtils.setAttribute(sess, newObj, typeLWS, parentLWS.getInts());
				}
			} catch (Exception e) {
				// エラー時は何もしない
			}
		}
	}

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;
	boolean sessPutFlag = false;

	//Parameter
	String prmObjId[] = request.getParameterValues("objId");
	String prmParentObjId[] = request.getParameterValues("parentObjId");
	String prmPasteType[] = request.getParameterValues("pasteType");
	String prmIsDocumentLink[] = request.getParameterValues("isDocumentLink");
	String prmFromParentObjId[] = request.getParameterValues("fromParentObjId");

	//Message
	String message = null;
	ArrayList paramIdList = new ArrayList();
	for(int i = 0 ; i < prmObjId.length ; i++)
	{
		paramIdList.add("objId[" + i + "]=" + prmObjId[i]);
		paramIdList.add("parentObjId[" + i + "]=" + prmParentObjId[i]);
		paramIdList.add("pasteType=[" + i + "]=" + prmPasteType[i]);
		paramIdList.add("isDocumentLink[" + i + "]=" + prmIsDocumentLink[i]);
		paramIdList.add("fromParentObjId[" + i + "]=" + prmFromParentObjId[i]);
	}
	Object[] paramId = paramIdList.toArray();

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

		// ThreadLocalコンテキストにセッションを設定
		if(EIMThreadContext.getEIMSession() == null)
		{
			EIMThreadContext.putEIMSession(sess);
			sessPutFlag = true;
		}

		//User
		user = (EIMUser)sess.getAttribute("USER");

		// TransactionContextの作成、設定
		if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
		{
			jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(user));

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		// リンクリレーションタイプの取得
		EIMRelationType relTypeLink = helper.getRelationTypeOfDocLink();

		// 所属ワークスペース関連処理クラス
		BelongingWSUtils belongingWSUtils = new BelongingWSUtils();

		// 作成したオブジェクトのMap<key:オブジェクトID, value:種類(ドキュメント、リンク、フォルダ、タグ)>
		Map<Long, String> createObjMap = new HashMap<Long, String>();

		// 渡されてきたオブジェクト分ループ
		for( int ii = 0; ii < prmObjId.length; ii++ ) {
			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId[ii]));
			if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ))
			{
				if( prmIsDocumentLink[ii].equals("true") && prmPasteType[ii].equals("COPY") ) {	// コピー＆ドキュメントリンクの場合
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				}
				else {	// ドキュメント or フォルダの場合 or タグの場合
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				}
				return;
			}

			//Object Type For Parent Recurrently
			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());

			//Parent Object
			EIMObject parentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmParentObjId[ii]));
			if(parentObj == null || !SecurityUtils.authorized(sess, parentObj, sess.getUser(),EIMAccessRole.READ))
			{
				if( prmIsDocumentLink[ii].equals("true") && prmPasteType[ii].equals("COPY") ) {	// ドキュメントリンクの場合
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDERORDOCUMENT");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDERORDOCUMENT");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				}
				else {	// ドキュメント or フォルダの場合 or タグの場合
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				}
				return;
			}

			// ペースト先がゴミ箱か否かをチェック
			if (AppObjectUtil.isObjectInRecycle(sess, parentObj)) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.PASTETORECYCLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.INPUT.PASTETORECYCLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}

			boolean isDocument = helper.isTypeOfDocument(object.getType());
			boolean isTag = helper.isTypeOfTag(object.getType());
			if( isTag ) {
				// タグの場合はペースト先がWF付フォルダ or コピー先がWF付フォルダ配下であればエラー
				// ※ 本メッセージは「コピー→貼付け」「切取り→貼付け」で共通
				if ((helper.isTypeOfFolderWithWorkflow(parentObj) ||
						helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj))) {
					throw new EIMException(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.TAG");
				}
			}

			//Parent Object Path
			String path = AppObjectUtil.getPath(parentObj);
			if(path == null){
				// ワークスペースの場合、パス属性の値を保持していない
				path = "/";
			}
			path += parentObj.getName() + "/";

			//Relation Type
			EIMRelationType relType = helper.getRelationTypeOfDocument();

			//Format
			EIMFormat formatPublic = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFormat formatSignEnc = null;
			if (OptionConfData.getInstance().SignAndEncrFlg) {
				formatSignEnc = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));
			}
			EIMFormat format = FileUtils.getDefaultFormat(sess, objType);

			//New Object Name
			String newObjName = object.getName();

			// [Copy]
			if(prmPasteType[ii].equals("COPY"))
			{
				// コピー元オブジェクトのセキュリティのチェック
				if (object.getSecurity() != null) {

					// ドキュメントの場合、「常時読取」以上ならばOK
					if (isDocument && !prmIsDocumentLink[ii].equals("true")) {
						if (!SecurityUtils.authorized(sess, object, sess.getUser(), AppConstant.ACCESS_ROLE_ALWAYS_READ)) {
							// コピー/貼付け権限がありません。
							throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCOPYNDPASTEROLE");
						}

					// ドキュメントリンク、または、タグの場合、「公開読取」以上ならばOK
					} else if ((isDocument && prmIsDocumentLink[ii].equals("true")) || isTag) {
						if (!SecurityUtils.authorized(sess, object,sess.getUser(), EIMAccessRole.READ)) {
							// コピー/貼付け権限がありません。
							throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCOPYNDPASTEROLE");
						}
					}
				}

				// 貼付け先オブジェクトのセキュリティのチェック
				if (parentObj.getSecurity() != null) {
					if (!SecurityUtils.authorized(sess, parentObj,sess.getUser(), EIMAccessRole.CREATE)) {
						// コピー/貼付け権限がありません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCOPYNDPASTEROLE");
					}
				}

				// コピー先のステータスの確認
				// ドキュメント or ドキュメントリンクの場合 (タグの場合はチェック済み)
				if( isDocument ) {
					// コピー先のステータス種別ID
					long stsKind = parentObj.getStatus() != null ? parentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;

					// 上位WFフォルダのステータスのチェック
					if ((helper.isTypeOfFolderWithWorkflow(parentObj)
							|| helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj))
								&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {	// 上位WFのステータスが「編集中」以外の場合はエラー
						// コピー/貼付け権限がありません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCOPYNDPASTEROLE");
					}
				}

				// 以下複製処理
				if( prmIsDocumentLink[ii].equals("true") ) {	// ドキュメントリンクの場合
					// [09/02/04 modified by ik.]
					// リンク作成(コピー)処理を AppLogicUtil に移動

					// コピー元のリンク更新タイミングを取得する。
					EIMObject fromParentObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmFromParentObjId[ii]));
					if(fromParentObj == null) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CUTPARENT.NOTFOUND");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.CUTPARENT.NOTFOUND");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}
					EIMRelation linkRel = RelationUtils.getRelationByParentAndChild(sess, relTypeLink, fromParentObj, object);
					EIMAttribute attr = linkRel.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
					long linkUpdateTiming = attr.getInt();

					AppLogicUtil.createDocLink(object, parentObj, true, true, helper, linkUpdateTiming);
					createObjMap.put((long)object.getId(), "documentLink");

					// ### SEARCH FRAMEWORK 検索FW更新通知 リンク元オブジェクト、貼り付け先オブジェクトID・処理種別キーを指定
					AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_COPY_DOCUMENTLINK");
					AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_PASTE_DEST_FOLDER", "SEARCHFW_PASTE_DEST_WORKSPACE", null, helper);
				}
				else if ( isTag ) {		// タグの場合

					//Create Object
					EIMObject newObj = ObjectUtils.createObject(sess, object.getType(), newObjName);
					long newObjId = newObj.getId();
					createObjMap.put(newObjId, "tag");

					//Create Relation
					while(true) {
						try {
							RelationUtils.createRelation(sess, relType, parentObj, newObj, EIMConstant.DEPU_CHECK_NAME_REV);
						} catch(EIMException ecp) {
							String errMessageKey = ecp.getMessageKey();
							if(errMessageKey.equals("EIM.ERROR.LOGIC.OBJECT.NAME.DUPLICATE")) {
								String langId = sess.getLangId();
								newObjName = EIMConfig.get("COPY_FILENAME_PREFIX_" + langId)
										+ " - " + newObjName;

								ObjectUtils.rename(sess, newObj, newObjName);
								continue;
							}
						}
						break;
					}

					//Inherit Attribute
					ObjectAttributeUtils.inheritAttribute(sess, object, newObj);

					// 所属ワークスペース、リンク所属ワークスペースは親オブジェクトの値を引き継ぐ
					belongingWSUtils.SetBelongingWS(sess, newObj, parentObj);

					// 属性情報の更新処理
					// (コピー＆ペースト処理はコピーで新規生成したオブジェクトが対象なので、↑のように事前にチェック処理を行う)
					newObj = ObjectUtils.getObjectById(sess, newObjId);
					AttributeUtil.updateAttributeForCopy(sess, newObj, parentObj, path);

					// 表示色情報のコピー
					DisplayColorUtil.copyDisplayColor(sess, newObj, object);

					// コピー元タグが付与されているドキュメント・フォルダ・タグについて、
					// コピー先タグの付与を実施する。
					AppLogicUtil.copyAssignedTag(sess, object, newObj, user, helper);

					// ### SEARCH FRAMEWORK 検索FW更新通知 コピーしたオブジェクト、貼り付け先オブジェクトID・処理種別キーを指定
					AppUpdateNoticeUtils.updateNoticeInsert(newObj.getId(), "SEARCHFW_COPY_TAG");
					AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_PASTE_DEST_FOLDER", "SEARCHFW_PASTE_DEST_WORKSPACE", null, helper);

					//Access
					AccessUtils.createAccess(sess, newObj, "EIM.ACCESS.TYPE.INITIALREGIST");

					//Create Operation History
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.COPY,
							EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, object,
							null, null, null, path);
				}
				else if( isDocument ) {	// ドキュメントの場合

					//Create Object
					EIMObject newObj = ObjectUtils.createObject(sess, object.getType(), newObjName);
					long newObjId = newObj.getId();
					createObjMap.put(newObjId, "document");

					//Create Relation
					while(true) {
						try {
							RelationUtils.createRelation(sess, relType, parentObj, newObj, EIMConstant.DEPU_CHECK_NAME_REV);
						} catch(EIMException ecp) {
							String errMessageKey = ecp.getMessageKey();
							if(errMessageKey.equals("EIM.ERROR.LOGIC.OBJECT.NAME.DUPLICATE")) {
								String langId = sess.getLangId();
								newObjName = EIMConfig.get("COPY_FILENAME_PREFIX_" + langId)
										+ " - " + newObjName;

								ObjectUtils.rename(sess, newObj, newObjName);
								continue;
							}
						}
						break;
					}

					//Inherit Attribute
					ObjectAttributeUtils.inheritAttribute(sess, object, newObj);

					// 所属ワークスペース、リンク所属ワークスペースは親オブジェクトの値を引き継ぐ
					belongingWSUtils.SetBelongingWS(sess, newObj, parentObj);

					// 属性情報の更新処理
					// (コピー＆ペースト処理はコピーで新規生成したオブジェクトが対象なので、↑のように事前にチェック処理を行う)
					newObj = ObjectUtils.getObjectById(sess, newObjId);

					// コピー元、先のObjectDomainを取得
					ObjectService objectService =
							(ObjectService)ApplicationContextLoader.getApplicationContext().getBean("documentObjectService2");
					ObjectDomain orgObjectDomain = objectService.getById(object.getId());
					ObjectDomain newObjectDomain = objectService.getById(newObj.getId());

					// 各属性タイプの内、オブジェクト型属性、ユーザ型属性を引継ぎ先に設定する
					List<AttributeDomain> attList = orgObjectDomain.getAttributeList();
					for (int i = 0; i < attList.size(); i++) {
						// Attribute
						AttributeDomain att = attList.get(i);

						// Attribute Type
						AttributeTypeDomain attType = att.getAttributeType();

						// 属性タイプのデータ型がOBJECT,USERでない場合はコピー対象外
						if (attType.getValueType() != ValueTypeEnum.OBJECT &&
								attType.getValueType() != ValueTypeEnum.USER) {
							continue;
						}

						if (attType.getValueType() == ValueTypeEnum.OBJECT) {

							// オブジェクト型の場合

							// 属性値を引き継ぐ
							List<ObjectDomain> attributeObjects = att.getObjectList();

							// 添付ファイルはコピー対象外
							if (attributeObjects.get(0).getType().getDefinitionName().equals(
									EIMConfig.getValue("OBJECT_TYPE_NAME_FORM_ATTACH_FILE"))) {
								continue;
							}

							// オブジェクト型属性値をコピー
							if (attType.isMultiple()) {
								objectService.setAttributeMultipleObject(newObjectDomain, attType, attributeObjects);
							}
							else {
								objectService.setAttributeSingleObject(newObjectDomain, attType, attributeObjects.get(0));
							}
						}
						else if (attType.getValueType() == ValueTypeEnum.USER) {


							// ユーザ型の場合

							// ユーザ型属性値をコピー
							if (attType.isMultiple()) {
								objectService.setAttributeMultipleUser(newObjectDomain, attType, att.getUserList());
							}
							else {
								objectService.setAttributeSingleUser(newObjectDomain, attType, att.getUserList().get(0));
							}
						}
					}

					// 表示色情報のコピー（下位引継ぎの前に行う）
					DisplayColorUtil.copyDisplayColor(sess, newObj, object);

					AttributeUtil.updateAttributeForCopy(sess, newObj, parentObj, path);

					// OCR
					if(OptionConfData.getInstance().ocrFlg){
						// OCR処理に関連する属性・オブジェクトを引き継ぐ
						AppOcrUtil.copyOcrStatus(sess, object, newObj);
					}

					// コピーされたオブジェクトに発番したIDをセットする
					// 連続データIDの取得
					String nextValue = AppObjectTypeUtil.getNextValue(newObj.getType().getId());
					if (nextValue != null) {
						AppObjectUtil.setAttrForce(sess, newObj, "番号", nextValue);
					}

					// 物理ファイルのコピー
					// Inherit Files without Public
					FileUtils.inheritFile(sess, object, newObj);
					EIMFile filePublic = FileUtils.getFile(sess, newObj, formatPublic);
					if (filePublic != null)
					{
						FileUtils.deleteFile(sess, newObj, formatPublic);
						//実ファイル削除
						File dstFile = new File(formatPublic.getDirectory().getPath() + FileUtils.getFileName(newObj, filePublic));
						dstFile.delete();
					}

					// WFなしドキュメントの場合、原本ファイルをコピーして公開ファイルとして登録
					if (!helper.isTypeOfDocumentWithWorkflow(newObj)) {
						EIMFile file = FileUtils.getFile(sess, newObj, format);
						File orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(newObj, file));
						File dstFile = new File(formatPublic.getDirectory().getPath() + newObj.getId() + file.getExt());
						FileUtils.createSymbolicLink(orgFile, dstFile);
						FileUtils.checkin(sess, newObj, formatPublic, newObjName, file.getSize());
					}

					// 署名・暗号化ファイルは継承しない
					if (OptionConfData.getInstance().SignAndEncrFlg) {
						EIMFile signEncFile = FileUtils.getFile(sess, newObj, formatSignEnc);
						if (signEncFile != null)
						{
							File substance = new File(signEncFile.getDirectory().getPath() + FileUtils.getFileName(newObj, signEncFile));
							if(substance.exists())
							{
								substance.delete();
							}
							FileUtils.deleteFile(sess, newObj, formatSignEnc);
						}
					}


					//Rename File
					FileUtils.renameFile(sess, newObj, format, newObjName);

					// ### SEARCH FRAMEWORK 検索FW更新通知 コピーしたオブジェクト、貼り付け先オブジェクトID・処理種別キーを指定
					AppUpdateNoticeUtils.updateNoticeInsert(newObj.getId(), "SEARCHFW_COPY_DOCUMENT");
					AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_PASTE_DEST_FOLDER", "SEARCHFW_PASTE_DEST_WORKSPACE", null, helper);

					//Access
					AccessUtils.createAccess(sess, newObj, "EIM.ACCESS.TYPE.INITIALREGIST");

					//Create Operation History
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.COPY,
							EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, object,
							null, null, null, path);
				}
			}

			// [Cut]
			else if(prmPasteType[ii].equals("CUT"))
			{
				if( prmIsDocumentLink[ii].equals("true") ) {	// ドキュメントリンクの場合

					// 切り取り元Object
					EIMObject fromObj = ObjectUtils.getObjectById(sess, Long.parseLong(prmFromParentObjId[ii]));
					if(fromObj == null) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CUTPARENT.NOTFOUND");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.CUTPARENT.NOTFOUND");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}

					// 切り取り対象オブジェクトのセキュリティのチェック
					if (object.getSecurity() != null) {
						if (!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
							// 切り取り/貼付け権限がありません。
							throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
						}
					}

					// 貼付け先オブジェクトのセキュリティのチェック
					if (parentObj.getSecurity() != null) {
						if (!SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.CREATE)) {
							// 切り取り/貼付け権限がありません。
							throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
						}
					}

					// 切り取り元のステータス種別ID
					long stsKind = fromObj.getStatus() != null ? fromObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
					// 上位WFフォルダのステータスのチェック
					if ((helper.isTypeOfFolderWithWorkflow(fromObj)
							|| helper.isTypeOfFolderUnderFolderWithWorkflow(fromObj))
								&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {	// 上位WFのステータスが「編集中」以外の場合はエラー
						// コピー/貼付け権限がありません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
					}

					// 貼り付け先のステータス種別ID
					stsKind = parentObj.getStatus() != null ? parentObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
					// 上位WFフォルダのステータスのチェック
					if ((helper.isTypeOfFolderWithWorkflow(parentObj)
							|| helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj))
								&& stsKind != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {	// 上位WFのステータスが「編集中」以外の場合はエラー
						// コピー/貼付け権限がありません。
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOCUTANDPASTEROLE");
					}

					// リンクリレーションの重複チェック
					if( RelationUtils.getRelationByParentAndChild(sess, relTypeLink, parentObj, object) != null ) {
						throw new EIMException(sess, "EIM.ERROR.LOGIC.DOCLINK.DUPLICATE");
					}

					// オブジェクトの「パス」「リンク先」属性に対して、以下を行う
					//	(1) 切り取り元オブジェクト情報を削除
					//	(2) 貼り付け先オブジェクト情報を追加
					int ret = AppLogicUtil.replaceDocLinkPath(sess, object, Long.parseLong(prmFromParentObjId[ii]), Long.parseLong(prmParentObjId[ii]), path);
					if( ret == AppLogicUtil.REPLACE_DOCLINK_PATH_ERROR_DOCLINK_MOVED ) {	// 既に移動済みである場合
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.DOCLINK.MOVED");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.DOCLINK.MOVED");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}

					// 切り取り対象Objectと切り取り元Objectとの間のリンクリレーションを取得
					EIMRelation relTmp = RelationUtils.getRelationByParentAndChild(sess, relTypeLink, fromObj, object);
					if( relTmp == null ) {	// 既に移動済みである場合
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.DOCLINK.MOVED");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.DOCLINK.MOVED");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}
					if(!SecurityUtils.authorized(sess, parentObj, sess.getUser(),EIMAccessRole.DELETE_RELATION)){
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODELETEROLE");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODELETEROLE");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}

					EIMAttribute attr = relTmp.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"));
					long linkUpdateTiming = attr.getInt();

					// 切り取り対象Object-切り取り元Object間のリレーションを削除→切り取り対象Object-貼り付け先Object間のリレーションを追加
					RelationUtils.deleteRelation(sess, relTmp);
					if( linkUpdateTiming == AppConstant.LINK_UPDATE_TIMING_AUTO ) {
						boolean flag = AppLogicUtil.isDuplicateLinkRelationAuto(helper, parentObj, object, null, null);
						if( flag ) {
							throw new EIMException(helper.getSession(), "EIM.ERROR.LOGIC.DOCLINK.AUTOLINK.EXIST");
						}
					}
					EIMRelation relation = RelationUtils.createRelation(sess, relTypeLink, parentObj, object);
					AppLogicUtil.setAttributeLinkRelation(sess, relation, linkUpdateTiming);
					createObjMap.put((long)object.getId(), "documentLink");

					// ### SEARCH FRAMEWORK
					// 対象がごみ箱内か判定
					boolean isParentRecycleBox = AppObjectUtil.isObjectInRecycleWithoutRecycle(sess, object);

					// ### SEARCH FRAMEWORK 検索FW更新通知 ドキュメントリンク元、処理種別キーを指定
					AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CUTPASTE_DOCUMENTLINK");
					if( !isParentRecycleBox )
					{
						// ### SEARCH FRAMEWORK 検索FW更新通知 切り取り元、処理種別キーを指定(切り取り元がゴミ箱以外)
						AppUpdateNoticeUtils.updateNoticeInsertParent(sess, fromObj, "SEARCHFW_CUTPASTE_ORG_FOLDER", "SEARCHFW_CUTPASTE_ORG_WORKSPACE", null, helper);
					}
					AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_PASTE_DEST_FOLDER", "SEARCHFW_PASTE_DEST_WORKSPACE", null, helper);

					//Access
					AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.DOCLINK.MOVE");

					//Create Operation History
					OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.DOCLINK_MOVE,
								AppConstant.LINK_ORIGIN, EIMConstant.OBJECT, object,
								EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, parentObj, path);
				}
				else {	// ドキュメントorフォルダorタグの場合

					// 対象がごみ箱内か判定
					boolean isParentRecycleBox = AppObjectUtil.isObjectInRecycleWithoutRecycle(sess, object);

					// 性能向上のためフォルダ配下のオブジェクトを一括ロードする
					AppObjectConditionHelper.LoadingModeEnum loadingMode = isParentRecycleBox ?
							AppObjectConditionHelper.LoadingModeEnum.VERSION_AND_BRANCH : AppObjectConditionHelper.LoadingModeEnum.VERSION;
					List<Integer> roleIdList = Arrays.asList(EIMAccessRole.READ, EIMAccessRole.UPDATE, AppConstant.ACCESS_ROLE_ALWAYS_READ);
					helper.loadChildObjectsRecursive(object, roleIdList, loadingMode);

					// 選択対象がタグが付与されたドキュメントorフォルダで、貼付け先がワークフロー付きフォルダの場合はエラー
					// ※ タグが付与されたタグはここに到達する前にエラーになる (タグそのものがWF付フォルダ配下にCOPY・MOVEできない)
					if ( AppLogicUtil.isTagAssignedObject(sess, object, helper, true)
							&& (helper.isTypeOfFolderWithWorkflow(parentObj) ||
								helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj)))
					{
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTMOVETAGGEDDOCFOLORTAG");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTMOVETAGGEDDOCFOLORTAG");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}

					// 配下にタグがあるフォルダで、貼付け先がワークフロー付きフォルダの場合はエラー
					if ( AppLogicUtil.isTagExistUnderFolder(sess, object, helper)
							&& (helper.isTypeOfFolderWithWorkflow(parentObj) ||
								helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj)))
					{
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.TAG");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTSET.UNDERWF.TAG");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}

					// 貼付け先フォルダが自分自身の場合はエラー
					if (parentObj.getId() == object.getId()) {
						// 貼付け先のフォルダと選択フォルダが同じです。
						message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NOTPASTESAMEFOLDER");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTPASTESAMEFOLDER");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}

					// 貼付け先フォルダが切り取ったフォルダに属する場合はエラー
					if (AppObjectUtil.isChildFolder(sess, parentObj, object, helper)) {
						// 貼付け先のフォルダは、選択フォルダに属するフォルダです。
						message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NOTPASTEFOLDER");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.INPUT.NOTPASTEFOLDER");
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}

					// Parent Relation
					List parentRelList = RelationUtils.getParentRelationListByRelType(sess, object, relType, EIMAccessRole.READ);

					// 親オブジェクト
					EIMObject[] orgParentList = new EIMObject[parentRelList.size()];
					for(int i = 0; i < parentRelList.size(); i++)
					{
						orgParentList[i] = ((EIMRelation)parentRelList.get(i)).getParent();
					}

					// 対象がワークスペース固有ごみ箱内か判定
					boolean isParentWSRecycleBox = false;

					for (int i = 0; i < orgParentList.length; i++) {

						if(orgParentList[i].getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACERECYCLE"))){
							isParentWSRecycleBox = true;
						}
					}

					// 属性情報の更新処理
					AttributeUtil.updateAttributeForMove(sess, object, parentObj, orgParentList[0], path, isParentRecycleBox, helper);

					// Delete Parent Relation
					for(int i = 0; i < parentRelList.size(); i++)
					{
						RelationUtils.deleteRelation(sess, (EIMRelation)parentRelList.get(i));
					}

					// Create Relation
					RelationUtils.createRelation(sess, relType, parentObj, object, EIMConstant.DEPU_CHECK_NAME_REV);

					if (isDocument) {
						createObjMap.put((long)object.getId(), "document");
					} else if (isTag) {
						createObjMap.put((long)object.getId(), "tag");
					} else {
						createObjMap.put((long)object.getId(), "folder");
					}

					// Access and Create Operation History
					if(isParentRecycleBox == true ){

						/* ブランチ情報の復帰 */
						EIMRelationType relTypeBranch = helper.getRelationTypeOfBranch();

						// フォルダの場合
						if(helper.isTypeOfFolder(object.getType())){
							AppLogicUtil.returnFolderBranch(sess, object, relTypeBranch, relType, helper);
						}
						// ドキュメントの場合
						else if( isDocument ) {
							AppLogicUtil.returnDocumentBranch(sess, object, relTypeBranch, helper);
						}

						if (isParentWSRecycleBox) {
							// ワークスペース固有ごみ箱からの復帰
							AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.BACKFROMWSRECBOX");
							OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.BACK_FROM_WORKSPACERECYCLEBOX,
									EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
									null, null, null, path);
						} else {
							// システムのごみ箱からの復帰
							AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.BACKFROMRECBOX");
							OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.BACK_FROM_RECYCLEBOX,
									EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
									null, null, null, path);
						}

					} else {

						// ### SEARCH FRAMEWORK 検索FW更新通知 切り取り元、処理種別キーを指定(切り取り元がゴミ箱以外)
						AppUpdateNoticeUtils.updateNoticeInsertParent(sess, orgParentList[0], "SEARCHFW_CUTPASTE_ORG_FOLDER", "SEARCHFW_CUTPASTE_ORG_WORKSPACE", null, helper);

						AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.MOVE");
						OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.MOVE,
								EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
								null, null, null, path);
					}

					// ### SEARCH FRAMEWORK 検索FW更新通知 対象ドキュメントorフォルダorタグのID、処理種別キーを指定
					AppUpdateNoticeUtils.updateNoticeInsertObject(sess, object, "SEARCHFW_CUTPASTE_FOLDER", "SEARCHFW_CUTPASTE_DOCUMENT", "SEARCHFW_CUTPASTE_TAG", null, true, helper);
					if(helper.isTypeOfFolder(object.getType()))
					{
						// ### SEARCH FRAMEWORK 検索FW更新通知 配下オブジェクト、処理種別キーを指定
						AppUpdateNoticeUtils.updateNoticeInsertChildRecursive(sess, object, "SEARCHFW_CUTPASTE_CHILD_FOLDER", "SEARCHFW_CUTPASTE_CHILD_DOCUMENT", "SEARCHFW_CUTPASTE_CHILD_TAG", "SEARCHFW_CUTPASTE_CHILD_DOCUMENTLINK", helper);
					}
					// 貼り付け先Object
					AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_PASTE_DEST_FOLDER", "SEARCHFW_PASTE_DEST_WORKSPACE", null, helper);
				}
			}

			// [Branch Copy]
			else if(prmPasteType[ii].equals("BRANCH_COPY")){
				// コピー元オブジェクトの権限チェック
				//   読み込み権限のみの場合は不可
				if(helper.isReadOnlyAccess(object)){
					// ブランチコピー/貼付け権限がありません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NOBRUNCHCOPYPASTEROLE");
				}
				//   WFなし、公開済み以外の場合は不可
				if(object.getStatus() != null && object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC){
					// ブランチコピー/貼付け権限がありません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NOBRUNCHCOPYPASTEROLE");
				}

				// 貼り付け先オブジェクトの権限チェック
				//   作成権限のチェック
				if(!SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.CREATE)){
					// ブランチコピー/貼付け権限がありません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NOBRUNCHCOPYPASTEROLE");
				}
				//ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
				if (parentObj.getStatus() != null && parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING){
					// ブランチコピー/貼付け権限がありません。
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NOBRUNCHCOPYPASTEROLE");
				}

				// 新規にオブジェクトを生成
				EIMObject newObj = ObjectUtils.createObject(sess, object.getType(), newObjName);
				long newObjId = newObj.getId();
				createObjMap.put(newObjId, "document");

				// 「ドキュメント」リレーションの生成
				while(true) {
					try {
						RelationUtils.createRelation(sess, relType, parentObj, newObj, EIMConstant.DEPU_CHECK_NAME_REV);
					} catch(EIMException ecp) {
						String errMessageKey = ecp.getMessageKey();
						if(errMessageKey.equals("EIM.ERROR.LOGIC.OBJECT.NAME.DUPLICATE")) {
							String langId = sess.getLangId();
							newObjName = EIMConfig.get("COPY_FILENAME_PREFIX_" + langId)
									+ " - " + newObjName;

							ObjectUtils.rename(sess, newObj, newObjName);
							continue;
						}
					}
					break;
				}

				// 「ブランチ」リレーションの生成
				EIMRelationType relTypeBranch = helper.getRelationTypeOfBranch();
				RelationUtils.createRelation(sess, relTypeBranch, object, newObj, EIMConstant.DEPU_CHECK_NONE);// リレーションの生成

				// 属性情報の継承
				ObjectAttributeUtils.inheritAttribute(sess, object, newObj);

				// 表示色情報のコピー
				DisplayColorUtil.branchcopyDisplayColor(sess, newObj, object);

				// 属性情報の更新処理
				// (コピー＆ペースト処理はコピーで新規生成したオブジェクトが対象なので、↑のように事前にチェック処理を行う)
				newObj = ObjectUtils.getObjectById(sess, newObjId);
				AttributeUtil.updateAttributeForCopy(sess, newObj, parentObj, path);

				// OCR
				if(OptionConfData.getInstance().ocrFlg){
					// OCR処理に関連する属性・オブジェクトを引き継ぐ
					AppOcrUtil.copyOcrStatus(sess, object, newObj);
				}

				// 物理ファイルのコピー
				// Inherit Files without Public
				FileUtils.inheritFile(sess, object, newObj);
				EIMFile filePublic = FileUtils.getFile(sess, newObj, formatPublic);
				if (filePublic != null)
				{
					FileUtils.deleteFile(sess, newObj, formatPublic);
					//実ファイル削除
					File dstFile = new File(formatPublic.getDirectory().getPath() + FileUtils.getFileName(newObj, filePublic));
					dstFile.delete();
				}

				// WFなしドキュメントの場合、原本ファイルをコピーして公開ファイルとして登録
				if (!helper.isTypeOfDocumentWithWorkflow(newObj)) {
					EIMFile file = FileUtils.getFile(sess, newObj, format);
					File orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(newObj, file));
					File dstFile = new File(formatPublic.getDirectory().getPath() + newObj.getId() + file.getExt());
					FileUtils.createSymbolicLink(orgFile, dstFile);
					FileUtils.checkin(sess, newObj, formatPublic, file.getName(), file.getSize());
				}

				// 署名・暗号化ファイルは継承しない
				if (OptionConfData.getInstance().SignAndEncrFlg) {
					EIMFile signEncFile = FileUtils.getFile(sess, newObj, formatSignEnc);
					if (signEncFile != null)
					{
						File substance = new File(signEncFile.getDirectory().getPath() + FileUtils.getFileName(newObj, signEncFile));
						if(substance.exists())
						{
							substance.delete();
						}
						FileUtils.deleteFile(sess, newObj, formatSignEnc);
					}
				}

				// ファイル名称のリネーム
				FileUtils.renameFile(sess, newObj, format, newObjName);

				// ### SEARCH FRAMEWORK 検索FW更新通知 ブランチ元・ブランチ先・貼り付け先オブジェクトID・処理種別キーを指定
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_BRCOPYPASTE_ORG_DOCUMENT");
				AppUpdateNoticeUtils.updateNoticeInsert(newObj.getId(), "SEARCHFW_BRCOPYPASTE_DOCUMENT");
				AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, "SEARCHFW_PASTE_DEST_FOLDER", "SEARCHFW_PASTE_DEST_WORKSPACE", null, helper);

				// アクセス履歴の作成
				AccessUtils.createAccess(sess, newObj, "EIM.ACCESS.TYPE.BRUNCH.CREATE");

				// 操作履歴の生成
				OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.BRUNCH_COPY,
						AppConstant.BRUNCH_COPY_ORIGIN, EIMConstant.OBJECT, object,
						EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, newObj, path);
			}
		}

		// ペーストしたオブジェクトIDを返却
		out.println("<object>");
		for (Map.Entry<Long, String> e : createObjMap.entrySet()) {
			boolean isDocumentLink = e.getValue().equals("documentLink") ? true: false;
			boolean isDocument = e.getValue().equals("document") ? true: false;
			out.println("<object objId=\"" + e.getKey() + "\" isDocumentLink=\"" + isDocumentLink + "\" isDocument=\"" + isDocument + "\" />");
		}
		out.println("</object>");

		sess.commit();
	}
	catch(jp.co.ctc_g.eim.framework2.common.exception.EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
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
			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
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