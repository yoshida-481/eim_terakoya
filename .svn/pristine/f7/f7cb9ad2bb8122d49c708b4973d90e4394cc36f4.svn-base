<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

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
	String prmUserId = EIMUtils.getParameter(request, "userId");
	String prmFromTime = EIMUtils.getParameter(request, "fromTime");
	String prmToTime = EIMUtils.getParameter(request, "toTime");

	//Message
	String message = null;
	Object[] paramId = {
			"userId=" + prmUserId,
			"fromTime=" + prmFromTime,
			"toTime=" + prmToTime
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
		loginUser = (EIMUser)sess.getAttribute("USER");
		if(!AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_OPEHIST))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.NOTADMINISTRATOR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.NOTADMINISTRATOR");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		
		//Root Node
		out.println("<operationHistories>");
		
		//Parameter(From Date)
		Date fromDate = null;
		if(prmFromTime != null && prmFromTime != "")
		{
			fromDate = DateUtils.editExpirationDate(sess, 
					StringUtils.getDateFromString(prmFromTime, EIMResource.getMessage(sess, "EIM.FORMAT.DATETIME")));
		}

		//Parameter(From Date)
		Date toDate = null;
		if(prmToTime != null && prmToTime != "")
		{
			toDate = DateUtils.editExpirationDate(sess, 
					StringUtils.getDateFromString(prmToTime, EIMResource.getMessage(sess, "EIM.FORMAT.DATETIME")));
		}

		//Parameter(User ID)
		long userId = -1;
		if(prmUserId != null && prmUserId != "")
		{
			userId = Long.parseLong(prmUserId);
		}
		
		//Parameter(Limit)
		String limitNumStr = EIMConfig.get("GET_DATA_MAX_NUM");
		if(limitNumStr == null || limitNumStr == "")
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OPEHIST.LIMIT.UNKNOWN");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.OPEHIST.LIMIT.UNKNOWN");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message));
			return;
		}
		int limitNum = Integer.parseInt(limitNumStr);
		
		//Search Operation History
		List<EIMOperationHistory> opeHistList = OperationHistoryUtils.search(sess, userId, fromDate, toDate, limitNum);

		// EIMOperationHistoryにはユーザコードが含まれないため、
		// ユーザIDを元にユーザを検索し、該当ユーザ情報を取得する

		// ユーザIDのSetを作成
		Set<Long> userIdSet = new HashSet<>();
		for (EIMOperationHistory history : opeHistList) {
			userIdSet.add(Long.valueOf(history.getUserId()));
		}

		// ユーザIDとユーザ情報のマップを作成
		List<EIMUser> users = UserUtils.getUserByIds(sess, new ArrayList<Long>(userIdSet));
		Map<Long, EIMUser> userIdAndDomainMap = new HashMap<>();
		for (EIMUser user : users) {
			userIdAndDomainMap.put(Long.valueOf(user.getId()), user);
		}
			
		for(int i = 0; i < opeHistList.size(); i++)
		{
			//Operation History
			EIMOperationHistory opeHist = (EIMOperationHistory)opeHistList.get(i);

			//Date
			String acDate = DateUtils.getDBTzToCLTzDate(sess, opeHist.getAccessDate(), "EIM.FORMAT.DATETIME");

			//User Code
			EIMUser user = userIdAndDomainMap.get(Long.valueOf(opeHist.getUserId()));
			String userCode = user != null ? user.getCode() : "";

			//Record Info A
			String rcInfo_A = "-";
			if(opeHist.getRecordInfoA() != null)
			{
				rcInfo_A = opeHist.getRecordInfoA();
			}

			//Record Type A
			String rcType_A = "-";
			if(opeHist.getRecordTypeA() != null)
			{
				rcType_A = opeHist.getRecordTypeA();
			}
			
			//Record Name A
			String rcName_A = "-";
			if(opeHist.getRecordNameA() != null)
			{
				rcName_A = opeHist.getRecordNameA();
			}
			
			//Record Info B
			String rcInfo_B = "-";
			if(opeHist.getRecordInfoB() != null)
			{
				rcInfo_B = opeHist.getRecordInfoB();
			}

			//Record Type B
			String rcType_B = "-";
			if(opeHist.getRecordTypeB() != null)
			{
				rcType_B = opeHist.getRecordTypeB();
			}
			
			//Record Name B
			String rcName_B = "-";
			if(opeHist.getRecordNameB() != null)
			{
				rcName_B = opeHist.getRecordNameB();
			}

			//Detail
			String detail = "-";
			if(opeHist.getDetail() != null)
			{
				detail = opeHist.getDetail();
			}

			//XML
			out.println("<operationHistory");
				out.println(" userCode=\"" + StringUtils.xmlEncode(userCode) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(opeHist.getUserName()) + "\"");
				out.println(" appType=\"" + StringUtils.xmlEncode(opeHist.getApplicationType()) + "\"");
				out.println(" acdate=\"" + StringUtils.xmlEncode(acDate) + "\"");
				out.println(" opType=\"" + StringUtils.xmlEncode(opeHist.getOperationType()) + "\"");
				out.println(" rcInfo_A=\"" + StringUtils.xmlEncode(rcInfo_A) + "\"");
				out.println(" rcType_A=\"" + StringUtils.xmlEncode(rcType_A) + "\"");
				out.println(" rcId_A=\"" + opeHist.getRecordIdA() + "\"");
				out.println(" rcName_A=\"" + StringUtils.xmlEncode(rcName_A) + "\"");
				out.println(" rcInfo_B=\"" + StringUtils.xmlEncode(rcInfo_B) + "\"");
				out.println(" rcType_B=\"" + StringUtils.xmlEncode(rcType_B) + "\"");
				out.println(" rcId_B=\"" + opeHist.getRecordIdB() + "\"");
				out.println(" rcName_B=\"" + StringUtils.xmlEncode(rcName_B) + "\"");
				out.println(" detail=\"" + StringUtils.xmlEncode(detail) + "\"");
				out.println(">");
			out.println("</operationHistory>");
		}
		
		//End Root Node
		out.println("</operationHistories>");
		
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
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
