<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.app.document.business.util.AccesableSecurityUtil"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>

<%
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");
	
	//Session
	EIMSession sess = null;
	EIMUser loginUser = null;
	
	//Parameter
	String prmObjId = EIMUtils.getParameter(request, "objId");
	Boolean isFilter = Boolean.valueOf(EIMUtils.getParameter(request, "isFilter"));
	
	//Message
	String message = null;

	try
	{
		// アクセスセキュリティ絞り込みフラグ
		// trueの場合、アクセスセキュリティで絞り込んだ情報をクライアントに返却
		Boolean accessSecurityNarrowDownFlag = Boolean.valueOf(EIMConfig.get("ACCESS_SECURITY_NARROW_DOWN"));
		
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
		loginUser = (EIMUser)sess.getAttribute("USER");
		
		// V5APIを使用
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));

		// サービス取得
		ComplexService compService = (ComplexService)ApplicationContextLoader.getApplicationContext().getBean("complexServiceForSelectEntry");
		
		// 返却用複合グループリスト
		List<EIMComp> compList = new ArrayList<EIMComp>();
		
		// 複合グループリスト取得
		compList = CompUtils.getCompList(sess);
		
		
		// アクセスセキュリティ絞り込みフラグがtureかつドキュメント管理の場合、アクセスセキュリティで絞り込む
		// システム管理から本処理が呼び出されることがあるためisAppを設ける
		if(accessSecurityNarrowDownFlag && isFilter)
		{
			// 対象オブジェクト参照可能ユーザリスト
			List<UserDomain> accesableUserList = AccesableSecurityUtil.getAccesableUserList(Long.parseLong(prmObjId));
			
			// 複合グループリスト(全て)
			List<ComplexDomain> compListAll = compService.getList();
			
			// 複合グループユーザIDマップ(key:複合グループID、value:ユーザIDリスト)
			HashMap<Long, List<Long>> compUserIdMap = new HashMap<Long, List<Long>>();
			for(ComplexDomain comp: compListAll)
			{
				// 複合グループユーザIDマップのvalue生成
				List<UserDomain> compUserList = comp.getUserList();
				List<Long> compUserIdList = new ArrayList<Long>();
				for(UserDomain compUser: compUserList)
				{
					compUserIdList.add(compUser.getId());
				}
				// 複合グループユーザIDマップに格納
				compUserIdMap.put(comp.getId(), compUserIdList);
			}
			
			// 複合グループリスト(画面返却用)
			compList = new ArrayList<EIMComp>();
			List<Long> compIdList = new ArrayList<Long>();
			for(UserDomain accesableUser: accesableUserList)
			{
				for(ComplexDomain comp: compListAll)
				{
					if(compUserIdMap.get(comp.getId()).contains(accesableUser.getId()))
					{
						// 複合グループのユーザリストにアクセス可能ユーザが含まれている場合、画面返却用リストに格納
						if(!compIdList.contains(comp.getId()))
						{
							compList.add(new EIMComp(comp.getId(),
										 			 new EIMGroup(comp.getGroup().getId(), comp.getGroup().getName(), null),
							 						 new EIMRole(comp.getRole().getId(), comp.getRole().getName(), null)));
							compIdList.add(comp.getId());
						}
					}
				}
			}
		}
		
		
		//Root Node
		out.println("<compList>");
		
		for(int i = 0; i < compList.size(); i++)
		{
			//Comp
			EIMComp comp = (EIMComp)compList.get(i);
			
			//XML
			out.println("<comp");
				out.println(" label=\"" + StringUtils.xmlEncode(comp.getName()) + "\"");
				out.println(" isBranch=\"" + "false" + "\"");
				out.println(" compId=\"" + comp.getId() + "\"");
			out.println(">");
			out.println("</comp>");
		}
		
		//End Root Node
		out.println("</compList>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage()), eime);
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
		try
		{
			if(EIMThreadContext.getTransactionContext() != null)
			{
				EIMThreadContext.removeTransactionContext();
			}
			if(sess != null)
			{
				sess.close();
			}
		}
		catch (Exception se)
		{
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
%>