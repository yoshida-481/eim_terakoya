<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@page import="eim.bo.*"%>
<%@page import="eim.net.*"%>
<%@page import="eim.util.*"%>

<%@page import="common.util.*"%>

<%@page import="java.util.*"%>

<%@page import="org.apache.commons.logging.*"%>

<%@page import="app.document.approve.ApproveCommonUtil"%>
<%@page import="jp.co.ctc_g.eim.app.document.common.util.AppWorkFlowUtil"%>

<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@page import="jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@page import="jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//Message
	String message = null;
	
	//Parameter
	String prmObjId = request.getParameter("objId");
	Object[] paramId = {
			"objId=" + prmObjId
			};
	
	boolean sessPutflg = false;
	
	try{
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		
		user = (EIMUser)sess.getAttribute("USER");
		
		//前処理
		if(EIMThreadContext.getEIMSession() == null)
		{
			//Service、Dao呼出に必要
			EIMThreadContext.putEIMSession(sess);
			sessPutflg = true;
		}

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
		
		//チェックand取得
		Map docObjTypeIdMap = AppObjectTypeUtil.getObjTypeMap(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		if(prmObjId == null || prmObjId.length() == 0)
		{
			// ドキュメント指定無し
		}
			
		//対象オブジェクト取得
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}
		
		// ドキュメント・WF付きフォルダのチェック
		boolean isDocument = (docObjTypeIdMap.get(new Long(object.getType().getId())) != null);
		if (isDocument)
		{
			// Document Type
			long higherFolderId = AppObjectUtil.getIntAttr(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);
			if (higherFolderId != -1)
			{
				//上位WFありDoc
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}
		else
		{
			// Folder Type
			if (! AppObjectUtil.isWFFolder(sess, object)){
				//WFなしフォルダ
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(object.getName())});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
		}

		//ステータスチェック 編集中のみ通過
		EIMStatus status = object.getStatus();
		if ( status == null || (status.getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTUPDATING", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
			return;
		}
		//ロック状態チェック
		if (object.getLockUser() != null || object.getLockDate() != null) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OBJECT.LOCKED", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OBJECT.LOCKED", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
			return;
		}
		
		
		//ワークフローとワークフロー設定オブジェクト取得
		EIMWorkFlow workflow = WorkFlowUtils.getWorkFlowByStatusType(sess, status.getType());
		if( workflow == null )
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
			return;
		}
		EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), String.valueOf(workflow.getId()));
		if( wfSettingObj == null )
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.WORKFLOW.NOTFOUND");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
			return;
		}

		// 承認不要WF/要承認WFのチェック
		String bossApproval = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_BOSS_APPROVAL_FLG"));
		if( bossApproval.equals("necessary") )
		{
			// 要承認WFは対象外
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOT.UNNECESSARY.WORKFLOW", new Object[]{object.getName()});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOT.UNNECESSARY.WORKFLOW", new Object[]{object.getName()});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
			return;
		}

		// 公開権限(ステータス変更権限)があるかをチェックする
		if(SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.STATUS_UP) != true)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOPUBLICROLE");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOPUBLICROLE");
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
			return;
		}

		//essential attribute
		EIMAttributeType attTypeOfModifyUser = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER"));
		EIMAttributeType attTypeOfModifyDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
		EIMAttributeType attTypeOfCreateDate = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE"));
		EIMAttributeType attTypeOfFizeSize = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE"));

		String modifyDate = DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate());
		String createDate = DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate());

		//XML
		out.println("<object");
			out.println(" objId=\"" + object.getId() + "\"");
			out.println(" objTypeId=\"" + object.getType().getId() + "\"");
			out.println(" objTypeName=\"" + StringUtils.xmlEncode(object.getType().getDefName()) + "\"");
			out.println(" objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
			out.println(" rev=\"" + object.getRev() + "\"");
			out.println(" securityId=\"" + object.getSecurityId() + "\"");

			String createUserName = "";
			if (!isDocument)
			{
				createUserName = object.getCreateUser().getName();
			}
			if (object.getAttribute("作成者") != null)
			{
				createUserName = UserUtils.getUserById(sess, object.getAttribute("作成者").getInt()).getName();
			}
			out.println(" createUserName=\"" + StringUtils.xmlEncode(createUserName) + "\"");
			out.println(" createDate=\"" + createDate + "\"");
			out.println(" modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" modifyDate=\"" + modifyDate + "\"");

			out.println(" attType_" + attTypeOfModifyUser.getId() + "=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
			out.println(" attType_" + attTypeOfModifyDate.getId() + "=\"" + modifyDate + "\"");
			out.println(" attType_" + attTypeOfCreateDate.getId() + "=\"" + createDate + "\"");
			if (isDocument) {
				EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
				out.println(" attType_" + attTypeOfFizeSize.getId() + "=\"" + file.getSize() + "\"");
			}

			out.println(" isDocument=\"" + Boolean.toString(isDocument) + "\"");
			out.println(" order=\"" + (isDocument ? "1" : "0") + "\"");

			//フルパス
			if(AppObjectUtil.getPath(object) != null)
			{
				out.println(" fullPath=\"" + StringUtils.xmlEncode(AppObjectUtil.getPath(object))
						+ StringUtils.xmlEncode(object.getName()) + "\"");
			}

			//Load Attribute
			StatusAttributeUtils.load(sess, status);

			out.println(" statusId=\"" + status.getId() + "\"");
			out.println(" statusTypeId=\"" + status.getType().getId() + "\"");
			out.println(" statusTypeName=\"" + StringUtils.xmlEncode(status.getType().getDefName()) + "\"");

			EIMStatusType statusType = AppWorkFlowUtil.getNextStatusType(object, AppConstant.BASE_EVENT_TYPE_ID_APPROVAL);
			long forcastStatusTypeId = 0;
			forcastStatusTypeId = statusType.getId();
			out.println(" forcastStatusTypeId=\"" + (forcastStatusTypeId != 0 ? forcastStatusTypeId : "") + "\"");
			out.println(" statusMDateLong=\"" + status.getMDate().getTime() + "\"");

			//メール通知オブジェクト取得
			EIMObject mailNotification = ObjectUtils.getObjectByTypeAndName(sess, ObjectUtils.getObjectTypeByName(sess, EIMConfig.getValue("OBJECT_TYPE_NAME_MAIL_NOTIFY")), prmObjId);

			String publicComment = null;
			String pubNotifCSVName = "";
			String pubNotifCSVID = "";
			String immediateStr = "";

			// メール通知オブジェクトが存在し、かつ更新者がログインユーザと同一であれば、公開通知先デフォルト設定よりも優先する
			if (mailNotification != null && mailNotification.getModifyUser().getId() == sess.getUser().getId()) {
				// 公開通知コメント
				publicComment = AppObjectUtil.getTextAttr(sess, mailNotification, EIMConfig.get("ATTR_NAME_PUBLIC_COMMENT"));

				// 公開通知先
				String[] publisherIdList = AppObjectUtil.getStrAttrs(sess, mailNotification, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_NOTIFY_TO"));
				if (publisherIdList != null) {
					for (String publisherId : publisherIdList) {
						String publisherName = ApproveCommonUtil.getNameFromCode(sess, publisherId).replaceAll("\"", "\"\"");
						if (publisherName.equals("")) {
							// 空文字列の場合は無効ユーザの場合のため除外
							continue;
						}
						if (pubNotifCSVName.equals("")) {
							pubNotifCSVName = "\"" + publisherName;
							pubNotifCSVID = publisherId;
						} else {
							pubNotifCSVName = pubNotifCSVName + "\",\"" + publisherName;
							pubNotifCSVID = pubNotifCSVID + "," + publisherId;
						}
					}
					pubNotifCSVName = pubNotifCSVName + "\"";
				}

				// 公開通知タイミング
				long timing = AppObjectUtil.getIntAttr(sess, mailNotification, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_PUBLIC_TIMING"), -1);
				immediateStr = AppWorkFlowUtil.getMailNoticeImmediate((int)timing);

			} else {
				// 公開通知先
				// 公開通知先エントリー取得
				EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess,EIMConfig.get("OBJECT_TYPE_NAME_WFPUBNOTIFY"));
				EIMObject entry = ObjectUtils.getObjectByTypeAndName(sess,objType,Long.toString(workflow.getId()));

				// 公開通知先デフォルト設定フラグ取得
				boolean pubNotifyFlag = false;
				long pubNotify = AppObjectUtil.getIntAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_PUBLISHNOTIFY_FLG"), -1);
				if( pubNotify != -1 )
				{
					pubNotifyFlag = (pubNotify == AppConstant.FLAG_ON ? true : false );
				}

				// 公開通知先エントリーが存在し, 公開通知先デフォルト設定フラグON
				if( entry != null && pubNotifyFlag == true ) {
					EIMAttribute attrTypeId = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_TYPE"));
					EIMAttribute attrTypeObj = entry.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_NOTIFY_ENTRY_OBJ"));
					// エントリタイプID, 対象IDが存在
					if( attrTypeId != null && attrTypeObj != null ) {
						// 属性を取得
						long[] ids = TypeConvertUtils.convertToLongArray(attrTypeId.getInts());
						long[] types = TypeConvertUtils.convertToLongArray(attrTypeObj.getInts());
						// エントリリスト (後にソートの上 XML 化)
						List userList = new ArrayList();
						List groupList = new ArrayList();
						List roleList = new ArrayList();
						List compList = new ArrayList();
						for( int j = 0; j < ids.length; j++ ) {
							// 「公開読取権」のチェックは行わない。
							// (公開通知メール送信時にチェックする)
							if( ids[j] == EIMAccessEntryType.USER ) {
								EIMUser entUser = UserUtils.getUserById(sess,types[j]);
								userList.add(entUser);
							} else if( ids[j] == EIMAccessEntryType.GROUP ) {
								EIMGroup entGroup = GroupUtils.getGroupById(sess,types[j]);
								groupList.add(entGroup);
							} else if( ids[j] == EIMAccessEntryType.ROLE ) {
								EIMRole entRole = RoleUtils.getRoleById(sess,types[j]);
								roleList.add(entRole);
							} else if( ids[j] == EIMAccessEntryType.COMP ) {
								EIMComp entComp = CompUtils.getCompById(sess,types[j]);
								compList.add(entComp);
							}
						}
						if( groupList.size() > 0 ) {
							groupList = AppObjectUtil.getStrSortedList(groupList, "getName", true);
							for( Iterator ite = groupList.iterator(); ite.hasNext(); ) {
								if( pubNotifCSVID.length() > 0 ) {
									pubNotifCSVName = pubNotifCSVName + ",";
									pubNotifCSVID = pubNotifCSVID + ",";
								}
								EIMGroup grp = (EIMGroup)ite.next();
								pubNotifCSVName = pubNotifCSVName + "\"" + grp.getName().replaceAll("\"","\"\"") + "\"";
								pubNotifCSVID = pubNotifCSVID + "2:" + grp.getId();
							}
						}
						if( roleList.size() > 0 ) {
							roleList = AppObjectUtil.getStrSortedList(roleList, "getName", true);
							for( Iterator ite = roleList.iterator(); ite.hasNext(); ) {
								if( pubNotifCSVID.length() > 0 ) {
									pubNotifCSVName = pubNotifCSVName + ",";
									pubNotifCSVID = pubNotifCSVID + ",";
								}
								EIMRole role = (EIMRole)ite.next();
								pubNotifCSVName = pubNotifCSVName + "\"" + role.getName().replaceAll("\"","\"\"") + "\"";
								pubNotifCSVID = pubNotifCSVID + "3:" + role.getId();
							}
						}
						if( compList.size() > 0 ) {
							compList = AppObjectUtil.getStrSortedList(compList, "getName", true);
							for( Iterator ite = compList.iterator(); ite.hasNext(); ) {
								if( pubNotifCSVID.length() > 0 ) {
									pubNotifCSVName = pubNotifCSVName + ",";
									pubNotifCSVID = pubNotifCSVID + ",";
								}
								EIMComp comp = (EIMComp)ite.next();
								pubNotifCSVName = pubNotifCSVName + "\"" + comp.getName().replaceAll("\"","\"\"") + "\"";
								pubNotifCSVID = pubNotifCSVID + "4:" + comp.getId();
							}
						}
						if( userList.size() > 0 ) {
							userList = AppObjectUtil.getStrSortedList(userList, "getName", true);
							for( Iterator ite = userList.iterator(); ite.hasNext(); ) {
								if( pubNotifCSVID.length() > 0 ) {
									pubNotifCSVName = pubNotifCSVName + ",";
									pubNotifCSVID = pubNotifCSVID + ",";
								}
								EIMUser eUser = (EIMUser)ite.next();
								pubNotifCSVName = pubNotifCSVName + "\"" + eUser.getName().replaceAll("\"","\"\"") + "\"";
								pubNotifCSVID = pubNotifCSVID + "1:" + eUser.getId();
							}
						}
					}
				}
			}

			if (immediateStr.equals("")) {
				// メール通知方法のデフォルト設定
				immediateStr = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_MAIL_NOTICE_DFLT"));
			}

			out.println(" publicComment=\"" + StringUtils.xmlEncode(StringUtils.nullToBlank(publicComment)) + "\"");
			out.println(" publisherName=\"" +  StringUtils.xmlEncode(pubNotifCSVName) + "\"");
			out.println(" publisherId=\"" +  StringUtils.xmlEncode(pubNotifCSVID) + "\"");
			out.println(" immediate=\"" + AppConstant.MAILNOTICE_TIMING_OFF_STR + "\"");
			out.println(" immediatePublic=\"" + StringUtils.nullToBlank(immediateStr) + "\"");

			//受信確認
			long reply = 0;
			if (mailNotification != null)
			{
				reply = AppObjectUtil.getIntAttr(sess, mailNotification, EIMConfig.get("ATTR_NAME_MAIL_NOTIFY_CONFIRM_RECEIVE"), 0);
			}
			out.println(" reply=\"" + reply + "\"");

			//現在のスタータスのステータスタイプ種別
			out.println(" statusKind=\"" + status.getType().getKind() + "\"");
			long docThrough = AppWorkFlowUtil.getDocThrough(status.getType());
			out.println(" through=\"" + docThrough + "\"");

			/*
			 * 有効期限切れ判定
			 */
			boolean expiration = false;
			EIMAttribute expirationDate = object.getAttribute(helper.getAttrNameOfEffectDate());
			if (expirationDate != null) {
				expiration = DateUtils.judgeExpirationDate(sess, expirationDate.getDate());
			}
			out.println(" expiration=\"" + expiration + "\"");

		out.println(">");
		out.println("</object>");

	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage()), eime);
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
			//Remove Session from Thread Local Table
			if(sessPutflg == true){
				EIMThreadContext.removeEIMSession();
				sessPutflg = false;
			}
			if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
			{
				jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
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
