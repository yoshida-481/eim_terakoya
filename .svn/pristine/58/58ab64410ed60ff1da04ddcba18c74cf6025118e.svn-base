<%@ page contentType = "text/xml; charset=UTF-8" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "java.sql.*" %>

<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.util.*" %>

<%@ page import = "jp.co.ctc_g.eim.app.document.business.util.AccesableSecurityUtil"%>

<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.*" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils"%>

<%

class FilterUser {

	public  List<EIMUser> filterConditon(EIMSession sess,
										String[] searchCondition,
										List<EIMUser> userList,
										Map<Long, List<String>> groupMap,
										Map<Long, List<String>> roleMap) throws Exception {

		
		List<EIMUser> resultUserList = new ArrayList<EIMUser>();

		// ユーザリストのサイズ分回す(ID,かな,名称の判定)
		for (int t = 0; t < userList.size(); t++) {
			
			// 検索条件合致の判定フラグ
			boolean flag = true;

			// スペース区切りにした条件それぞれに対してId、かな、定義名称、グループ名称、ロール名称に合致するものがあるか判定する
			// 合致するものがある場合のみuserListに残す
			for (int i = 0; i < searchCondition.length; i++) {
				
				String nowCondition = searchCondition[i];
				
				// 検索条件が、ユーザコードと合致するとき
				if (String.valueOf(userList.get(t).getCode()).indexOf(nowCondition) != -1) {
					continue;
				}
				
				if(userList.get(t).getName() != null){
					// 検索条件が、表示名称と合致するとき
					if (userList.get(t).getName().indexOf(nowCondition) != -1) {
						continue;
					}
				}
				
				if (userList.get(t).getKana() != null) {
					// 検索条件が、かなと合致するとき
					if (userList.get(t).getKana().indexOf(nowCondition) != -1) {
						continue;
					}
				}

				long key = userList.get(t).getId();
				// 検索条件が、グループ名称と合致するとき

				boolean groupFlag = true;
				List<String> value = groupMap.get(key);
				if (value != null) {
					groupFlag = refineSearchConditon(value,nowCondition);
					
					if(groupFlag == true){
						continue;
					}
					
				}

				// 検索条件が、ロール名称と合致するとき

				boolean roleFlag = true;
				value = roleMap.get(key);
				if (value != null) {
					roleFlag = refineSearchConditon(value,nowCondition);
					
					if(roleFlag == true){
						continue;
					}
				}

				flag = false;
			}
		
			if(flag == true){
				resultUserList.add(userList.get(t));
			}

		}
		return resultUserList;
	}
	
	public  boolean refineSearchConditon(List<String> value,String nowCondition) throws Exception{
		
		boolean flag = true;
		
		for (int s = 0; s < value.size(); s++) {
			if (value.get(s).indexOf(nowCondition) == -1) {
				flag = false;
			}else{
				flag = true;
				return flag;
			}
		}
		
		return flag;
	}

}


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
	String prmUserCode = EIMUtils.getParameter(request, "userCode");
	String prmUserName = EIMUtils.getParameter(request, "userName");
	String prmUserKana = EIMUtils.getParameter(request, "userKana");
	Boolean isFilter = Boolean.valueOf(EIMUtils.getParameter(request, "isFilter"));
	String prmSearchConditon = EIMUtils.getParameter(request, "searchCondition");

	//Message
	String message = null;
	Object[] paramId = {
			"userCode=" + prmUserCode,
			"userName=" + prmUserName,
			"userKana=" + prmUserKana
			};
	
	
	CallableStatement cstmt_group = null;
	CallableStatement cstmt_role = null;
	ResultSet rset_group = null;
	ResultSet rset_role = null;
	
	List<EIMUser> userList = new ArrayList<EIMUser>();
	
	try
	{
		// アクセスセキュリティ絞り込みフラグ
		// trueの場合、アクセスセキュリティで絞り込んだ情報をクライアントに返却
		Boolean accessSecurityNarrowDownFlag = Boolean.valueOf(EIMConfig.get("ACCESS_SECURITY_NARROW_DOWN"));
		
		//Session
		sess = EIMUtils.getSession(request);
		// Connection
		Connection conn = sess.getDBConnection();
		
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}

		loginUser = (EIMUser)sess.getAttribute("USER");
		
		//SQL
		String sql_group = "";
		String sql_role = "";
		
		
		//SQLでユーザの属性、所属グループを取得する
		sql_group = "select USR.ID,USR.CODE,USR.NAME UNAME,USR.KANA,USR.MAIL,GP.GNAME from "
					+ "(select USR.ID,USR.CODE,uoth.NAME,USR.KANA,USR.MAIL from EIMUSEROTHER uoth,EIMUSER USR where USR.DISABLE = 0 and USR.ID = uoth.USID and uoth.LID = '"
					+ sess.getLangId()
					+ "') USR "
					+ "left outer join "
					+ "(select gasgn.ID,goth.NAME GNAME from EIMGASGN gasgn,EIMGROUPOTHER goth where gasgn.GID = goth.GID and goth.LID = '"
					+ sess.getLangId()
					+ "') GP "
					+ "on GP.ID = USR.ID order by UNAME";
		
		//SQLでユーザの属性、所属ロールを取得する
		sql_role = "select USR.ID,USR.CODE,USR.NAME UNAME,USR.KANA,USR.MAIL,RL.RNAME from "
					+ "(select USR.ID,USR.CODE,uoth.NAME,USR.KANA,USR.MAIL from EIMUSEROTHER uoth,EIMUSER USR where USR.DISABLE = 0 and USR.ID = uoth.USID and uoth.LID = '"
					+ sess.getLangId()
					+ "') USR "
					+ "left outer join "
					+ "(select rasgn.ID,roth.NAME RNAME from EIMRASGN rasgn,EIMROLEOTHER roth where rasgn.RID = roth.RID and roth.LID = '"
					+ sess.getLangId()
					+ "') RL "
					+ "on RL.ID = USR.ID order by UNAME";
		
		// Prepare
		cstmt_group = conn.prepareCall(sql_group);
		cstmt_role = conn.prepareCall(sql_role);
		// Execute
		rset_group = cstmt_group.executeQuery();
		rset_role = cstmt_role.executeQuery();
		
		//グループ名を保持するリスト
		List<String> groupNameList = new ArrayList<String>();
		//グループ用ユーザIdを保持するリスト
		List<String> userIdList_group = new ArrayList<String>();
		
		//ロール名を保持するリスト
		List<String> roleNameList = new ArrayList<String>();
		//ロール用ユーザIdを保持するリスト
		List<String> userIdList_role = new ArrayList<String>();
		
		//DBのグループを取得する
		Map<Long, List<String>> groupMap = new HashMap<Long, List<String>>();
		Map<Long, List<String>> roleMap = new HashMap<Long, List<String>>();
		
		HashSet userIdSet = new HashSet();
		
		
		//グループ側で実行した結果
		while (rset_group.next()) {
			
			long userId = rset_group.getLong("id");
			String userCode = rset_group.getString("code");
			String userName = rset_group.getString("uname");
			String userKana = rset_group.getString("kana");
			String userMail = rset_group.getString("mail");
			String langId = sess.getLangId();
			String groupName = rset_group.getString("gname");
			
			EIMUser eimUser = new EIMUser(userId,userCode,userName,userKana,null,userMail,0,0,null,langId);
			
			//既に処理したeimUserは追加しない
			if(userName != null){
				if(userIdSet.contains(eimUser.getId()) != true){
					userList.add(eimUser);
					userIdSet.add(eimUser.getId());
				}
			}
			
			
			//既に処理したuserIdの場合にはグループ名称をリストに追加する
			if(groupMap.get(userId) == null){
				groupNameList = new ArrayList<String>();
				if(groupName != null){
					groupNameList.add(groupName);
					groupMap.put(userId, groupNameList);
				}
			}else{
				if(groupName != null){
					groupMap.get(userId).add(groupName);
					groupMap.put(userId, groupNameList);
				}
			}
				
		}
		
		
		//ロール側で実行した結果
		while (rset_role.next()) {
			
			long userId = rset_role.getLong("id");
			String userCode = rset_role.getString("code");
			String userName = rset_role.getString("uname");
			String userKana = rset_role.getString("kana");
			String userMail = rset_role.getString("mail");
			String langId = sess.getLangId();
			String roleName = rset_role.getString("rname");
			
			EIMUser eimUser = new EIMUser(userId,userCode,userName,userKana,null,userMail,0,0,null,langId);
			
			//既に処理したeimUserは追加しない
			if(userName != null){
				if(userIdSet.contains(eimUser.getId()) != true){
					userList.add(eimUser);
					userIdSet.add(eimUser.getId());
				}
			}
			
			//既に処理したuserIdの場合にはロール名称をリストに追加する
			if(roleMap.get(userId) == null){
				roleNameList = new ArrayList<String>();
				if(roleName != null){
					roleNameList.add(roleName);
					roleMap.put(userId, roleNameList);
				}
			}else{
				if(roleName != null){
					roleMap.get(userId).add(roleName);
					roleMap.put(userId, roleNameList);
				}
			}
		}
		
		//画面表示用のグループ、ロールマップを生成する
		Map<Long, String> outputGroupMap = new HashMap<Long, String>();
		Map<Long, String> outputRoleMap = new HashMap<Long, String>();
		
		for(EIMUser user : userList){
			long userIdKey = user.getId();
			// グループ
			List<String> allGroupNameList = groupMap.get(userIdKey);
			String allGroupName = "";
			if(allGroupNameList != null && allGroupNameList.size() > 0){
				// ソート
				Collections.sort(allGroupNameList);
				// カンマ区切りのグループ名生成
				for(String groupName : allGroupNameList){
					if(allGroupName.equals("")){
						allGroupName = groupName;
					}else{
						allGroupName += "," + groupName;
					}
				}
			}
			outputGroupMap.put(userIdKey, allGroupName);
			
			// ロール
			List<String> allRoleNameList = roleMap.get(userIdKey);
			String allRoleName = "";
			if(allRoleNameList != null  && allRoleNameList.size() > 0){
				// ソート
				Collections.sort(allRoleNameList);
				// カンマ区切りのロール名生成
				for(String roleName : allRoleNameList){
					if(allRoleName.equals("")){
						allRoleName = roleName;
					}else{
						allRoleName += "," + roleName;
					}
				}
			}
			outputRoleMap.put(userIdKey, allRoleName);
		}
		
		//公開通知先参照画面で検索条件を入力したとき、検索結果から検索条件で絞込みを行う
		if (prmSearchConditon != null) {
			String[] searchCondition = prmSearchConditon.replaceAll("　", " ").split(" ", -1);
			
			// 検索条件に合致するユーザ情報を抽出
			FilterUser filterUser = new FilterUser();
			//検索条件で絞り込んだユーザのみ次の処理に渡す
			userList  = filterUser.filterConditon(sess, searchCondition, userList, groupMap, roleMap);
		
		}

		// 検索結果が0件の時
		if(userList.size() == 0)
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NORESULT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		
		// 検索結果が1000件より大きい時
		if(userList.size() > Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OVERFLOWRESULT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		
		// アクセスセキュリティ絞り込みフラグがtureかつドキュメント管理の場合、アクセスセキュリティで絞り込む
		// システム管理から本処理が呼び出されることがあるためisAppを設ける
		if(accessSecurityNarrowDownFlag && isFilter)
		{
			// 対象オブジェクト参照可能ユーザリスト
			List<UserDomain> accesableUserList = AccesableSecurityUtil.getAccesableUserList(Long.parseLong(prmObjId));
			
			// 対象オブジェクト参照可能ユーザリストを比較用にセットに格納
			HashSet<Long> accesableUserIdSet = new HashSet<Long>();
			for(UserDomain accesableUser: accesableUserList)
			{
				accesableUserIdSet.add(accesableUser.getId());
			}
			
			// 表示ユーザ情報を抽出
			List<EIMUser> searchUserList = new ArrayList<EIMUser>();
			for(EIMUser searchUser: userList)
			{
				if(accesableUserIdSet.contains(searchUser.getId()))
				{
					// 検索結果ユーザが対象オブジェクトへアクセス可能ユーザの場合、ユーザリストに追加
					searchUserList.add(searchUser);
				}
			}
			// セキュリティで絞り込んだユーザ情報のみ返却
			userList = searchUserList;
		}
		
		//Root Node
		out.println("<users>");
		
		for(int i = 0; i < userList.size(); i++)
		{
			//User
			EIMUser user = (EIMUser)userList.get(i);
			
			//かな
			String userKana = "";
			if(user.getKana() != null)
			{
				userKana = user.getKana();
			}
			
			//Mail
			String userMail = "";
			if(user.getMail() != null)
			{
				userMail = user.getMail();
			}
			
			long userId = user.getId();
			
			//Group
			String groupName = "";
			//グループに所属するユーザの場合にはグループを取得
			if( outputGroupMap.get(userId) != null){
				groupName = outputGroupMap.get(userId);
			}else{
				groupName = "";
			}
			//グループに所属するユーザの場合にはグループを取得
			
			//Role
			String roleName = "";
			//ロールに所属するユーザの場合にはロールを取得
			if( outputRoleMap.get(userId) != null){
				roleName = outputRoleMap.get(userId);
			}else{
				roleName = "";
			}
			//ロールに所属するユーザの場合にはロールを取得
			
			//XML
			out.println("<user");
				out.println(" userId=\"" + user.getId() + "\"");
				out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
				out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
				out.println(" groupName=\"" + StringUtils.xmlEncode(groupName) + "\"");
				out.println(" roleName=\"" + StringUtils.xmlEncode(roleName) + "\"");
				out.println(">");
			out.println("</user>");
		}
		
		//End Root Node
		out.println("</users>");
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
		try
		{
			if(cstmt_group != null){
				cstmt_group.close();
			}
			if(rset_group != null){
				rset_group.close();
			}
			if(cstmt_role != null){
				cstmt_role.close();
			}
			if(rset_role != null){
				rset_role.close();
			}
			if(sess != null){
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