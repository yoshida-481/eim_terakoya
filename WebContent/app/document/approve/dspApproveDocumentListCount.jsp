<%@ page contentType = "text/xml; charset=UTF-8" %>

<%@page import="eim.bo.*"%>
<%@page import="eim.net.*"%>
<%@page import="eim.util.*"%>

<%@page import="common.util.*"%>

<%@page import="java.util.*"%>

<%@page import="org.apache.commons.logging.*"%>
<%@ page import = "org.springframework.context.*" %>

<%@page import="app.document.approve.ApproveCommonUtil"%>
<%@page import="eim.bo.EIMStatusType"%>

<%@page import="jp.co.ctc_g.eim.framework.business.dao.AssignDao"%>
<%@page import="jp.co.ctc_g.eim.framework.business.dao.AssignPlanDao"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.AssignPlanDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.EventDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.ObjectDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.StatusDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.StatusTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.AssignDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.TaskDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.service.TaskService"%>
<%@page import="jp.co.ctc_g.eim.framework.common.util.EIMThreadContext"%>
<%@page import="jp.co.ctc_g.eim.framework.common.util.ApplicationContextLoader"%>
<%@page import="jp.co.ctc_g.eim.framework.business.service.WorkFlowDefService"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.WorkFlowDomain" %>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.entity.EventTypeDomain"%>
<%@page import="jp.co.ctc_g.eim.framework.business.domain.EventLogDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.entity.AttributeDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.domain.AssignDomain"%>
<%@ page import = "jp.co.ctc_g.eim.framework.business.dao.StatusDao"%>


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
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		List<EIMObject> resultList = new ArrayList<EIMObject>();
		TaskService taskService = null;
		
		//assignDao
		AssignDao assignDao = (AssignDao)ApplicationContextLoader.getContext().getBean("assignDao");
		
		//チェックand取得
		Map docObjTypeIdMap = AppObjectTypeUtil.getObjTypeMap(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
		if (prmObjId != null && prmObjId.length() > 0)
		{
			//対象オブジェクト取得
			EIMObject obj = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if(obj == null)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTEXIST.DOCFOL");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
				return;
			}
			
			//Check Document Type or Folder Type
			boolean isDocument = (docObjTypeIdMap.get(new Long(obj.getType().getId())) != null);
			if (isDocument)
			{
				// Document Type
				long higherFolderId = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER"), -1);
				if (higherFolderId != -1)
				{
					//上位WFありDoc
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[]{StringUtils.xmlEncode(obj.getName())});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXIST.HIGHRANK.WFFOLDER", new Object[]{StringUtils.xmlEncode(obj.getName())});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}
			else
			{
				// Folder Type
//				if (obj.getStatus() == null)
				if (! AppObjectUtil.isWFFolder(sess, obj)){
					//WFなしフォルダ
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(obj.getName())});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOSELECT.DOC.OR.WFFOLDER", new Object[]{StringUtils.xmlEncode(obj.getName())});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
					return;
				}
			}

			//ステータスチェック
			if (obj.getStatus() == null || (obj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_APPROVE))
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAPPROVING", new Object[]{obj.getName()});
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAPPROVING", new Object[]{obj.getName()});
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
				return;
			}

			//WorkFlow
			EIMWorkFlow workflowObj = WorkFlowUtils.getWorkFlowByStatusType(sess, obj.getStatus().getType());
			if(workflowObj != null)
			{
				//ワークフロー設定オブジェクト取得
				EIMObject wfSettingObj = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_WFSETTING"), "" + workflowObj.getId());

				//要承認WF/承認不要WFのチェック
				// 承認、承認待ち一覧の際、WFが承認不要WFならばエラー
				// bossApprovalがnullになるのは、要承認WFのみなのでエラーにしないこと。
				String bossApproval = AppObjectUtil.getStrAttr(sess, wfSettingObj, EIMConfig.get("ATTR_NAME_WFSETTING_BOSS_APPROVAL_FLG"));
				if( bossApproval!=null && !bossApproval.equals("necessary") )
				{
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOT.NECESSARY.WORKFLOW", new Object[]{obj.getName()});
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOT.NECESSARY.WORKFLOW", new Object[]{obj.getName()});
					log.warn(AppMessageUtils.makeLogMessage(user.getId(), message));
					return;
				}
			}

			//「ステータス変更」権限があるかをチェックする
			if(SecurityUtils.authorized(sess, obj,sess.getUser(), EIMAccessRole.STATUS_UP) != true)
			{
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPROVEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
				return;
			}
			
			
			// ログインユーザーのタスクリストを取得する
			List<EIMObject> taskObjectList = ApproveCommonUtil.getApproveRequestedTaskList(sess, user.getId());
			
			// タスクド存在チェック
			boolean findFlg = false;
			for(EIMObject taskObject : taskObjectList){
				long taskObjId = taskObject.getId();
				if(taskObjId == obj.getId()){
					findFlg = true;
					break;
				}
			}
			if(!findFlg) {
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOAPPROVEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOAPPROVEROLE");
				log.warn(AppMessageUtils.makeLogMessage(user.getId(), message))	;
				return;
			}
			
			resultList.add(obj);
			
		}
		else
		{
			resultList = ApproveCommonUtil.getApproveRequestedTaskList(sess, user.getId());
		}
		//Root Node
		out.println("<objList>");

		//Document
		for (Iterator<EIMObject> itr = resultList.iterator(); itr.hasNext();) {
			//EIMObject obj = (EIMObject) itr.next();
			EIMObject object = (EIMObject) itr.next();
			//必要な属性情報が入っていないので、再取得
			//EIMObject object = ObjectUtils.getObjectById(sess, obj.getId());
			//boolean isDocument = (docObjTypeIdMap.get(new Integer(object.getType().getId())) != null);
			
			//XML
			out.println("<object");
				out.println(" objId=\"" + object.getId() + "\"");
			out.println(" />");
		}
		
		//End Root Node
		out.println("</objList>");

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
