<%@page import="java.util.List"%>
<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>

<%@ page import = "org.apache.commons.logging.*" %>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());
	
	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser  user = null;

	//Parameter
	String prmObjId = request.getParameter("objId");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId
			};

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

		//Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENT");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		//アクセス権限チェック
		if (SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.CHECKIN) != true)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCHECKINROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKINROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
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
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
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

					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;

				// ステータスが「承認依頼中」の場合
				} else if (statusTypeKindId == AppConstant.STATUS_TYPE_KIND_ID_APPROVE) {
					// WF取得
					EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, object.getStatus().getType());

					// WFがない場合、エラー
					if (workflow == null) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOT.APPLIED.WORKFLOW.DOCUMENT", new Object[]{object.getName()});
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOT.APPLIED.WORKFLOW.DOCUMENT", new Object[]{object.getName()});
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
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
							message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.DISABLE.APPROVER.CHECKIN", new Object[]{object.getName(), EIMResource.getMessage("EIM.ACCESS.TYPE.CHECKIN")});
							out.println(AppMessageUtils.makeErrorTagByMessage(message));
							message = EIMResource.getMessage("EIM.ERROR.LOGIC.DISABLE.APPROVER.CHECKIN", new Object[]{object.getName(), EIMResource.getMessage("EIM.ACCESS.TYPE.CHECKIN")});
							log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
							return;
						}
					}

					// ログインユーザが現ステータスのエントリに入っているかチェック
					if (!AppWorkFlowUtil.isUserEntriedApproverCheck(object.getStatus().getId(), user.getId())) {
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANNOT.CHECKIN.ONLY.APPROVER", new Object[]{object.getName(), EIMResource.getMessage("EIM.ACCESS.TYPE.CHECKIN")});
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANNOT.CHECKIN.ONLY.APPROVER", new Object[]{object.getName(), EIMResource.getMessage("EIM.ACCESS.TYPE.CHECKIN")});
						log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
						return;
					}
					isApproverCheckin = true;
				}
			// WFなしの場合
			} else {
				if (object.getLockUser() != null) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}

			}
		}
		
		//WFなしドキュメントで、かつタグが付与されたドキュメントの場合はエラー
		if( object.getStatus() == null 
			|| object.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_NONE)
		{
			List aList = object.getAttributeList();
			for(int ii = 0; ii < aList.size(); ii++) {
				EIMAttribute a = (EIMAttribute)aList.get(ii);
				if(a.getType().getDefName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"))) {
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CHECKIN.TAGGEDDOC");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.CHECKIN.TAGGEDDOC");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
		}
		
		//チェックアウトしたかどうかでドキュメントタイプ変更可能かどうかを判定する
		//チェックアウトしない場合にはドキュメントタイプ変更不可
		boolean activatedFlag = false;

		//Check Lock User
		if(object.getRevision() > 0)
		{
			boolean isEditing = false;
			if (object.getStatus() != null) {
				// ステータスが「編集中」の場合
				long statusTypeKindId = object.getStatus().getType().getKind();
				if (statusTypeKindId == AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
					isEditing = true;
				}
			// WF無しの場合、チェックイン可能のためフラグON
			} else {
				isEditing = true;
			}

			//Version
			EIMVersion version = VersionUtils.getVersion(sess, object);
			
			//Lock Object
			EIMObject lockObj = version.getObjectByRev(object.getRevision() - 1);
			
			// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
			//過去ドキュメントがロックされている場合は、チェックアウトしてチェックインした場合である。
			if(lockObj != null && lockObj.getLockUser() != null && lockObj.getLockUser().getId() == user.getId() && isEditing){
				activatedFlag = true;
			}

			// 「承認依頼中」の場合
			if (isApproverCheckin) {
				// オブジェクトのlockuserの有無でロック状態を判断
				// 承認依頼中の場合、設定によりチェックイン可能なため
				// ※ユーザ・ステータス自体がチェックイン可能かはステータスチェックで行う
				if (object.getLockUser() != null) {

					message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJECT.LOCKED");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}

			// 「承認依頼中」以外の場合
			} else {
				// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
				// チェックアウト無しでチェックインする場合は、過去ドキュメントはロックされていない
				if (lockObj != null && lockObj.getLockUser() != null && lockObj.getLockUser().getId() != user.getId())
				{
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKOUTUSER");
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
			}
		} else {
			// オブジェクトがロックされている場合のみ、ユーザーの比較を行う
			if (object != null && object.getLockUser() != null && object.getLockUser().getId() != user.getId())
			{
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCHECKOUTUSER");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
		}
		// 直接編集でロックされている場合はチェックイン不可
		if (object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_WEBDAV_LOCK_FLAG")) != null) {
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJECT.LOCKED");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}

		//essential attribute
		EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));
		
		String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
		String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());
		String confirmDisplayFlag = EIMConfig.get("DOC_UPDATE_MESSAGE_INCLUDE");
		EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));

		//XML
		out.println("<object");
		
			//For Create Document - Default Create User Information
			out.println(" userId=\"" + user.getId() + "\"");
			out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
		
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" latest=\"" + object.getLatest() + "\"");
			out.println(" createUserName=\"" + StringUtils.xmlEncode(object.getCreateUser().getName()) + "\"");//作成者属性対応対象外:画面に表示されない
			out.println(" createDate=\"" + object.getCreateDate() + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + object.getModifyDate() + "\"");
			
			//Revision
			out.println(" rev=\"" + "-" + "\"");
			
			//パス
			String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
			out.println(" path=\"" + StringUtils.xmlEncode(path) + "\"");

			out.println(" attType_" + attTypeOfModifyUser.getId() + "=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate + "\"");
			out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate + "\"");
			out.println(" attType_" + attTypeOfFizeSize.getId() + "=\"" + file.getSize() + "\"");
			out.println(" activatedFlag =\"" + activatedFlag + "\"" );
			out.println(" checked = \"false\"");
			out.println(" confirmDisplayFlag =\"" + confirmDisplayFlag + "\"");

		out.println(">");
		out.println("</object>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), paramId), eime);
	}
	catch(Exception e)
	{
		out.clear();
		message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
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
