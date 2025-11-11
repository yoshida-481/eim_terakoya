<%@page import="jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain"%>
<%@ page contentType="text/html; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.io.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil" %>
<%@ page import = "jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.FormatDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.DirectoryDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.dao.FileDao"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.dao.ObjectDao"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.ObjectService"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.WorkflowDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.WorkflowService" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/html; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	boolean sessPutFlag = false;

	//Message
	String message = null;
	Object[] paramId = null;

	try
	{
		//Session
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

		//Login User
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

		//MultiPartFormUtils
		MultiPartFormUtils mpf = null;
		try{
			mpf = new MultiPartFormUtils(request);
		}
		catch(EIMException eime) {
			sess.rollback();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
			out.println(AppMessageUtils.makeErrorTagByMessage(eime.getMessage()));
			return;
		}
		//Parameter
		String prmObjId = mpf.getParameter("objId");
		String fileName = mpf.getParameter("fileName");
		String prmUpdateComment = mpf.getParameter("updateComment");
		String prmDestObjTypeId = mpf.getParameter("destObjTypeId");
		String prmIsReturnObjectId = mpf.getParameter("isReturnObjectId");
		paramId = new Object[]{
				"objId=" + prmObjId,
				"fileName=" + fileName,
				"updateComment=" + prmUpdateComment,
				"destObjTypeId=" + prmDestObjTypeId,
				"isReturnObjectId=" + prmIsReturnObjectId
				};

		boolean isReturnObjectId = false;
		if (prmIsReturnObjectId != null && prmIsReturnObjectId.equals("true")) {
			isReturnObjectId = true;
		}

		//File Name
		String fileExt = StringUtils.getFileExt(fileName);
		if(fileExt == null)
		{
			fileExt = "";
		}

		//ファイル存在チェック
		if(mpf.getFileSize() == 0)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPLOADFILE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
			// 複数ファイル登録の時は修正が必要
		}

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		//先に処理対象のドキュメントの同一バージョン内のオブジェクト全てに行ロックをかける
		List<EIMObject> objListInVersion = VersionUtils.getVersion(sess, object).getList();
		long[] objIds = new long[objListInVersion.size()];
		for(int i = 0; i < objListInVersion.size(); i++)
		{
			objIds[i] = objListInVersion.get(i).getId();
		}
		AppObjectUtil.lockObjectById(sess, objIds);
		// 処理待ち中にEIMObjectの値が変更された可能性があるので再取得
		object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			response.sendError(1006);
			return;
		}

		//作成権限のチェック
		if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.CHECKIN))
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKINROLE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// 承認依頼中チェックイン実行フラグ
		boolean isApproverCheckin = false;
		//ステータスのチェック
		// 承認中チェックイン許可設定がOFFの場合
		if (!OptionConfData.getInstance().enableApproverCheckin) {
			// WFなしドキュメントでもチェックインは出来るものとする
			if( object.getStatus() != null && object.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING
				|| ( object.getStatus() == null && object.getLockUser() != null ) )
			{
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				return;
			}
		// 承認中チェックイン許可設定がONの場合
		} else {
			// WFありの場合
			if (object.getStatus() != null) {
				// ステータスが「編集中」「承認依頼中」以外の場合、エラー
				long statusTypeKindId = object.getStatus().getType().getKind();
				if (statusTypeKindId != AppConstant.STATUS_TYPE_KIND_ID_EDITTING
					&& statusTypeKindId != AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {

					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					return;

				// ステータスが「承認依頼中」の場合
				} else if (statusTypeKindId == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
					// WF取得
					EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, object.getStatus().getType());

					// WFがない場合、エラー
					if (workflow == null) {
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOT.APPLIED.WORKFLOW.DOCUMENT", new Object[]{object.getName()});
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
						return;

					} else {
						//ワークフロー設定オブジェクト取得
						EIMObject workflowSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workflow.getId()));

						// 「チェックイン可能ステータス」属性を取得し、現在のステータスがチェックインを許可しているかをチェック
						boolean enableCheckStatus = false;
						EIMAttribute enableCheckinStatusAttr = workflowSettingObj.getAttribute(EIMConfig.get("ATTR_NAME_ENABLE_CHECKIN_STATUS"));
						if (enableCheckinStatusAttr != null) {
							long[] enableCheckinStatusArr = TypeConvertUtils.convertToLongArray(enableCheckinStatusAttr.getInts());
							for (int i = 0; i < enableCheckinStatusArr.length; i++) {
								if (object.getStatus().getType().getId() == enableCheckinStatusArr[i]) {
									enableCheckStatus = true;
								}
							}
						}
						// 現在のステータスがチェックインを許可していない場合、エラー
						if (!enableCheckStatus) {
							message = EIMResource.getMessage("EIM.ERROR.LOGIC.DISABLE.APPROVER.CHECKIN", new Object[]{object.getName(), EIMResource.getMessage("EIM.ACCESS.TYPE.CHECKIN")});
							out.println(AppMessageUtils.makeErrorTagByMessage(message));
							log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
							return;
						}
					}

					// ログインユーザが現ステータスのエントリに入っているかチェック
					if (!AppWorkFlowUtil.isUserEntriedApproverCheck(object.getStatus().getId(), loginUser.getId())) {
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANNOT.CHECKIN.ONLY.APPROVER", new Object[]{object.getName(), EIMResource.getMessage("EIM.ACCESS.TYPE.CHECKIN")});
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
						return;
					}

					isApproverCheckin = true;
				}
			// WFなしの場合
			} else {
				if (object.getLockUser() != null) {
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					return;
				}

			}
		}

		//Version
		EIMVersion version = VersionUtils.getVersion(sess, object);

		//対象が最新のリビジョンかのチェック
		if (object.getRevision() != version.getMaxRevision())
		{
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKINROLE");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		//Check Lock User
		if(object.getRevision() > 0)
		{
			// 「承認依頼中」の場合
			if (isApproverCheckin) {
				// オブジェクトのlockuserの有無でロック状態を判断
				// 承認依頼中の場合、設定によりチェックイン可能なため
				// ※ユーザ・ステータス自体がチェックイン可能かはステータスチェックで行う
				if (object.getLockUser() != null) {

					message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJECT.LOCKED");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}

			// 「承認依頼中」以外の場合
			} else {
				//Lock Object
				EIMObject lockObj = version.getObjectByRev(object.getRevision() - 1);
				// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
				// チェックアウト無しでチェックインする場合は、過去ドキュメントはロックされていない
				if (lockObj != null && lockObj.getLockUser() != null && lockObj.getLockUser().getId() != loginUser.getId())
				{
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKOUTUSER");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
			}
		} else {
			// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
			if (object != null && object.getLockUser() != null && object.getLockUser().getId() != loginUser.getId())
			{
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKOUTUSER");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
		}
		// 直接編集でロックされている場合はチェックイン不可
		if (object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG")) != null) {
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJECT.LOCKED");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		// 署名・暗号化中でないかチェック
		EIMFormat formatSignEnc = null;
		if (OptionConfData.getInstance().SignAndEncrFlg) {
			long signencr = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			if (signencr == AppConstant.SIGNENCR_KIND_PROCESSING_SIGNENCR)
			{
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANNOT.CHECKIN.WITH.SIGN.AND.ENCR.PROC");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			formatSignEnc = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_SIGNENCR"));
			if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR)
			{
				// 署名・暗号化済
				if (formatSignEnc == null)
				{
					// 署名・暗号化フォーマットが取得できない
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.SIGN.AND.ENCR.FORMAT.NOTFOUND");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
				else if (FileUtils.getFile(sess, object, formatSignEnc) == null)
				{
					// 署名・暗号化ファイルが取得できない
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NO.SIGN.AND.ENCR.FILE.WITHDOCNAME",
							new Object[]{object.getName()});
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
			}
		}


		//Attribute Type
		EIMAttributeType attType = AttributeUtils.getAttributeTypeByName(sess, "改訂内容");
		ObjectAttributeUtils.setAttribute(sess, object, attType, prmUpdateComment);

		//Format
		//EIMFormat format = FileUtils.getFormatByName(sess, "ドキュメント");
		EIMFormat format = FileUtils.getDefaultFormat(sess, object.getType());

		//Directory
		EIMDirectory dir = format.getDirectory();

		EIMFile file = FileUtils.getFile(sess, object, format);

		EIMRelation docRel = null;

		List relList = RelationUtils.getParentRelationListByRelType(sess, object, RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT")),EIMAccessRole.READ);

		if(!relList.isEmpty()) {
			docRel = (EIMRelation)relList.get(0);
		}

		//Rename
		//object = ObjectUtils.rename(sess, object, fileName);
		if(!fileName.equals(object.getName())){
			try{
				//名前変更する時の重複チェックする際、名前のみでチェックするように変更する
				//object = ObjectUtils.rename(sess, object, docRel, fileName, EIMConstant.DEPU_CHECK_NAME_REV);
				object = ObjectUtils.rename(sess, object, docRel, fileName, EIMConstant.DEPU_CHECK_NAME);
			} catch(EIMException eime) {
				sess.rollback();
				// 同じ親を持つ同名のオブジェクトが既に存在します。
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
				out.println(AppMessageUtils.makeErrorTagByMessage(eime.getMessage()));
				return;
			}
		}

		//Checkin
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}

		context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		ObjectDomain objectDomain = new ObjectDomain(object.getId());
		FileDomain fileDomain = new FileDomain();
		fileDomain.setDirectory(new DirectoryDomain(format.getDirectory().getId()));
		fileDomain.setFormat(new FormatDomain(format.getId()));
		fileDomain.setName(fileName);
		fileDomain.setSize(mpf.getFileSize());
		fileDomain.setSequence(0);

		FileDao fileDao = (FileDao)ApplicationContextLoader.getApplicationContext().getBean("fileDao2");

		fileDao.update(objectDomain,fileDomain);

		//変更前のオブジェクトタイプを保持
		EIMObjectType srcObjectType = object.getType();

		//変更後のオブジェクトタイプを取得
		EIMObjectType destObjectType = ObjectUtils.getObjectTypeById(sess, Long.parseLong(prmDestObjTypeId));
		//変更後タイプがWFありドキュメントか判定
		boolean wfFlag = true;
		//変更後タイプのWF保持用
		WorkflowDomain wf = null;
		//変更後のオブジェクトのOCR処理設定を保持
		boolean destOcrSettingFlag = false;

		//オブジェクトタイプの変更がある場合
		if(destObjectType.getId() != object.getType().getId()){

			ObjectService objectService = (ObjectService)ApplicationContextLoader.getApplicationContext().getBean("checkinObjectService");
			//変更元ドメイン
			ObjectDomain srcObjectDomain = objectService.getById(object.getId());
			//変更後ドメイン
			ObjectDomain destObjectDomain = new ObjectDomain();
			ObjectTypeDomain destObjectTypeDomain = new ObjectTypeDomain();
			destObjectDomain = srcObjectDomain;

			//変更後タイプ
			destObjectTypeDomain.setId(destObjectType.getId());
			//判定のため変更後タイプのWFを保持
			WorkflowService workflowService = (WorkflowService)ApplicationContextLoader.getApplicationContext().getBean("workflowService2");
			wf = workflowService.getByObjectType(destObjectTypeDomain);
			//変更後のタイプがWFなしドキュメントの場合はフラグを立てる
			if(wf == null){
				wfFlag = false;
			}

			//変更後オブジェクトドメインは変更前のオブジェクトドメインに変更後のタイプをつけたもの
			destObjectDomain.setType(destObjectTypeDomain);

			objectService.update(destObjectDomain);

			object.setType(destObjectType);

			//WFなしドキュメントの場合はステータスを削除
			if(wfFlag == false){
				object.setStatus(null);
			}

		}

		//変更後のOCR処理設定
		destOcrSettingFlag = AppOcrUtil.hasOcrSetting(sess, object);

		//タイプのチェンジによりOCR処理設定が変更の場合はOCR処理ステータスを変更する
		if(destOcrSettingFlag == true){
			// OCR処理ステータスを「0:処理待ち」に更新
			AppOcrUtil.setOcrProcessingStatus(sess, object, AppConstant.OCR_PROC_STATUS_PROCESS_WAIT);
		}else if(destOcrSettingFlag == false){
			// OCR処理ステータスを削除する
			AppOcrUtil.setOcrProcessingStatus(sess, object, AppConstant.OCR_PROC_STATUS_NONE);
		}


		//Upload
		mpf.upload(dir.getPath() + object.getId() + fileExt);

		EIMObject latestObj = null;

		//WFなしドキュメントの場合、公開ドキュメントの置換え
		if (object.getStatus() == null && wf == null) {

			// 公開ドキュメントとして登録
			EIMFormat publicDocumentFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
			EIMFile baseFile = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));

			File orgFile = new File(baseFile.getDirectory().getPath() + FileUtils.getFileName(object, baseFile));
			File dstFile = new File(publicDocumentFormat.getDirectory().getPath() + object.getId() + baseFile.getExt());
			FileUtils.createSymbolicLink(orgFile, dstFile);
			FileUtils.checkin(sess, object, publicDocumentFormat, baseFile.getName(), baseFile.getSize());

			//Delete Relation For Old Revision
			if(object.getRev() > 0)
			{
				//Set Latest
				object = VersionUtils.setLatest(sess, object);

				//Old Revision
				latestObj = version.getObjectByRev(object.getRev() - 1);

				//Unlock (これを実行しないとステータスが「改訂中」のまま残る)
				ObjectUtils.unLock(sess, latestObj);

				//Parent Relation
				List parentRelList = RelationUtils.getParentRelationListByRelType(sess, latestObj, RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT")),EIMAccessRole.READ);

				//Delete Relation
				for(int i = 0; i < parentRelList.size(); i++)
				{
					EIMRelation relation = (EIMRelation)parentRelList.get(i);
					RelationUtils.deleteRelation(sess, relation);

					// マルチインスタンス暫定対応
					// 過去リビジョンのドキュメントを検索条件にヒットさせるため、リレーションは
					// 削除するが「所属ワークスペース」属性の値は削除しない（トリガ起動後なので再設定する）
					EIMAttributeType attrTypeBelongWS = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"));
					if(attrTypeBelongWS != null){
						EIMObject parent = relation.getParent();
						long parentBelongWS = Integer.MIN_VALUE;
						// 親がワークスペースである場合
						if(parent.getType().getDefName().equals(EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE"))){
							parentBelongWS = parent.getId();
						}else{
							parentBelongWS = AppObjectUtil.getIntAttr(sess, relation.getParent(), EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"), Integer.MIN_VALUE);
						}

						if(parentBelongWS != Integer.MIN_VALUE){
							AppObjectUtil.setAttr(sess, relation.getChild(), EIMConfig.getValue("ATTR_NAME_DOCUMENT_BELONGING_WS"), parentBelongWS);
						}
					}
				}

				// 前リビジョンがロックされている（チェックアウトされているドキュメントにチェックインを行う）場合
				if(latestObj.getLockUser() != null) {
					// リンク自動更新処理を呼び出す。
					// チェックアウト無しでチェックインする場合は、リンク自動更新処理は呼び出さない。
					// リンクリレーションの子オブジェクト（ドキュメント）を最新版のドキュメントに設定する
					UpdateObjectLinkUtil.actUpdateObjectLinkByDocument(sess, object);

					// OCR
					if(OptionConfData.getInstance().ocrFlg){
						// OCR処理ステータスが「0:処理待ち」の場合、OCR処理オブジェクトを作成する
						if(AppOcrUtil.getOcrProcessingStatus(sess, object) == AppConstant.OCR_PROC_STATUS_PROCESS_WAIT){
							AppOcrUtil.createOcrProcessingObject(sess, object);
						}
					}

				}

				// SearchFramework 検索FW更新通知 対象：全リビジョンドキュメント
				AppUpdateNoticeUtils.updateNoticeInsert(latestObj.getId(), "SEARCHFW_CHECK_IN_OLD_DOCUMENT");
			}
		}

		//
		// 元のオブジェクトから、署名・暗号化に関する属性、ファイルを削除する
		//
		if (OptionConfData.getInstance().SignAndEncrFlg) {
			AppObjectUtil.setAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			AppObjectUtil.deleteAttribute(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER"));
			object = ObjectUtils.getObjectById(sess, object.getId());
			EIMFile signEncFile = FileUtils.getFile(sess, object, formatSignEnc);
			if (signEncFile != null)
			{
				File substance = new File(signEncFile.getDirectory().getPath() + FileUtils.getFileName(object, signEncFile));
				if(substance.exists())
				{
					substance.delete();
				}
				FileUtils.deleteFile(sess, object, formatSignEnc);
			}
		}


		//パス
		String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));

		//変更元と変更先のドキュメントタイプ
		String changeType = srcObjectType.getName() + "→" + destObjectType.getName();

		// SearchFramework 検索FW更新通知 対象：ドキュメント
		AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CHECK_IN_DOCUMENT");

		//Create Operation History
		if(srcObjectType.getId() != object.getType().getId()){
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, EIMConstant.TYPE_CHANGE,
					EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
					null, null, null, changeType);

			//Access
			AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.CHANGE");
		}

		String accType = "EIM.ACCESS.TYPE.CHECKIN";
		String opeType = EIMConstant.CHECKIN;
		if (isApproverCheckin) {
			accType = "EIM.ACCESS.TYPE.CHECKIN.APPROVER";
			opeType = AppConstant.CHECKIN_APPROVER;
		}

		//Access
		AccessUtils.createAccess(sess, object, accType);

		OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, opeType,
				EIMConstant.TARGET_UPDATE, EIMConstant.OBJECT, object,
				null, null, null, path);

		// XML
		if (isReturnObjectId) {
			// 最新版のオブジェクトを再取得する
			object = ObjectUtils.getObjectById(sess, object.getId());
			boolean objHasWF = (object.getStatus() != null ? true : false);
			out.println("<object objId=\"" + object.getId() + "\"" + " objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"" + " objHasWF=\"" + objHasWF + "\"");
			if (latestObj != null) {
				boolean previousObjHasWF = (latestObj.getStatus() != null ? true : false);
				out.println(" previousObjId=\"" + latestObj.getId() + "\" previousObjHasWF=\"" + previousObjHasWF + "\"");
				out.println(" unlock=\"" + (latestObj.getLockUser() != null ? "true" : "false") + "\"");
			}
			if (!objListInVersion.isEmpty()) {
				StringBuilder pastObjId = new StringBuilder();
				for (EIMObject objInVersion : objListInVersion) {
					if (objInVersion.getId() == object.getId()) {
						continue;
					}
					if (pastObjId.length() > 0) {
						pastObjId.append(",");
					}
					pastObjId.append(Long.toString(objInVersion.getId()));
				}
				out.println(" pastObjId=\"" + pastObjId.toString() + "\"");
			}
			out.println(">");
			out.println("</object>");
		}

		sess.commit();

		// 拡張子が変わる場合は変更前のファイルを削除
		// ロールバックが発生した場合、原本ファイルを復元できないため、コミットが終了したのちファイルを削除する。
		if (!file.getExt().equals(fileExt)) {
			File target = new File(dir.getPath() + FileUtils.getFileName(object, file));
			target.delete();
		}

	}
	catch(jp.co.ctc_g.eim.framework2.common.exception.EIMException eime)
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
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
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
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
		}
	}
%>
