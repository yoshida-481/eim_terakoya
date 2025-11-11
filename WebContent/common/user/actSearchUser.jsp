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
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.UserCriteria"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.UserService"%>
<%

class FilterUser {

	public  List<UserDomain> filterConditon(EIMSession sess,
										String[] searchCondition,
										List<UserDomain> userList,
										Map<Long, List<String>> groupMap,
										Map<Long, List<String>> roleMap) throws Exception {

		
		List<UserDomain> resultUserList = new ArrayList<UserDomain>();

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
	String objId = EIMUtils.getParameter(request, "objId");
	String keyword = EIMUtils.getParameter(request, "keyword");
	Boolean securityFilter = Boolean.valueOf(EIMUtils.getParameter(request, "securityFilter"));
	Boolean disableUserFilter = Boolean.valueOf(EIMUtils.getParameter(request, "disableUserFilter"));
	Boolean systemUserFilter = Boolean.valueOf(EIMUtils.getParameter(request, "systemUserFilter"));

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + objId,
			"keyword=" + keyword,
			"securityFilter=" + securityFilter,
			"disableUserFilter=" + disableUserFilter,
			"systemUserFilter=" + systemUserFilter
			};


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
		
		// トランザクションコンテキスト
		if(EIMThreadContext.getTransactionContext() != null)
		{
			EIMThreadContext.removeTransactionContext();
		}
		TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
		EIMThreadContext.putTransactionContext(context);
		context.setLangId(sess.getLangId());
		context.setDBConnection(sess.getDBConnection());
		context.setUser(ConvertUtils.toUserDomain(loginUser));
		
		// 検索条件の設定
		UserCriteria userCriteria = new UserCriteria();
		// 有効ユーザのみ検索対象とする
		userCriteria.setDisable(false);
		// ユーザ検索の実行
		UserService userService = (UserService)ApplicationContextLoader.getApplicationContext().getBean("userServiceForUserSearch");
		List<UserDomain> userList = userService.getList(userCriteria);

		Map<Long, List<String>> groupMap = new HashMap<Long, List<String>>();
		Map<Long, List<String>> roleMap = new HashMap<Long, List<String>>();

		for(UserDomain user : userList){
			long userId = user.getId();
			//グループ名を保持するリスト
			List<String> groupNameList = new ArrayList<String>();
			//ロール名を保持するリスト
			List<String> roleNameList = new ArrayList<String>();
			
			// 多言語名称が存在しない場合、定義名称を設定する。
			if (user.getName() == null) {
				OtherNameDomain otherNameDomain = new OtherNameDomain();
				otherNameDomain.setLangId(context.getLangId());
				otherNameDomain.setName(user.getDefinitionName());
				user.getNameList().add(otherNameDomain);
			}
			
			for (GroupDomain group : user.getGroupList()) {
				if (group.getName() != null) {
					groupNameList.add(group.getName());
				} else {
					groupNameList.add(group.getDefinitionName());
				}
			}
			if (groupNameList.size() > 0) {
				groupMap.put(userId, groupNameList);
			}
			
			for (RoleDomain role : user.getRoleList()) {
				if (role.getName() != null) {
					roleNameList.add(role.getName());
				} else {
					roleNameList.add(role.getDefinitionName());
				}
			}
			if (roleNameList.size() > 0) {
				roleMap.put(userId, roleNameList);
			}
		}
		
		// キーワードで絞り込む
		if (keyword != null) {
			String[] keywords = keyword.replaceAll("　", " ").split(" ", -1);
			
			// 検索条件に合致するユーザ情報を抽出
			FilterUser filterUser = new FilterUser();
			//検索条件で絞り込んだユーザのみ次の処理に渡す
			userList  = filterUser.filterConditon(sess, keywords, userList, groupMap, roleMap);
		
		}

		// 検索結果が0件の時
		if(userList.size() == 0)
		{
			out.println("<users></users>");
			return;
		}
		
		// 検索結果が1000件より大きい時
		if(userList.size() > Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM")))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.OVERFLOWRESULT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			return;
		}
		
		// 設定ファイルの権限絞り込みフラグがtrue かつ リクエストパラメータsecurityFilterがtrueの場合
		//if(accessSecurityNarrowDownFlag && securityFilter) {
		if(securityFilter){
			// 対象オブジェクト参照可能ユーザリスト取得
			List<UserDomain> accesableUserList = AccesableSecurityUtil.getAccesableUserList(Long.parseLong(objId));
			// 対象オブジェクト参照可能ユーザリストを比較用にセットに格納
			HashSet<Long> accesableUserIdSet = new HashSet<Long>();
			for(UserDomain accesableUser: accesableUserList)
			{
				accesableUserIdSet.add(accesableUser.getId());
			}
			// 表示ユーザ情報を抽出
			List<UserDomain> searchUserList = new ArrayList<UserDomain>();
			for(UserDomain searchUser: userList)
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
			UserDomain user = (UserDomain)userList.get(i);
			// systemユーザチェック
			if( systemUserFilter && user.getId() == 1 ){
				continue;
			}
			
			// 無効ユーザチェック
			if( disableUserFilter && user.isDisable() ){
				continue;
			}
			
			// ID
			long userId = user.getId();
			
			// かな
			String userKana = "";
			if(user.getKana() != null){
				userKana = user.getKana();
			}
			
			// Mail
			String userMail = "";
			if(user.getMail() != null){
				userMail = user.getMail();
			}
			// Disable
			String userDisable = "off";
			if(user.isDisable()){
				userDisable = "on";
			}
			
			// Group
			String groupName = "";
			if (user.getGroupList() != null) {
				// 多言語名称がnullなら定義名称をセット
				for (GroupDomain group : user.getGroupList()) {
					if (group.getName() == null) {
						OtherNameDomain otherNameDomain = new OtherNameDomain();
						otherNameDomain.setLangId(context.getLangId());
						otherNameDomain.setName(group.getDefinitionName());
						
						group.getNameList().add(otherNameDomain);
						
					}
				}
				groupName = UserListUtil.toSeparatedGroupNames(user.getGroupList());
			}
			
			// Role
			String roleName = "";
			if (user.getRoleList() != null) {
				// 多言語名称がnullなら定義名称をセット
				for (RoleDomain role : user.getRoleList()) {
					if (role.getName() == null) {
						OtherNameDomain otherNameDomain = new OtherNameDomain();
						otherNameDomain.setLangId(context.getLangId());
						otherNameDomain.setName(role.getDefinitionName());
						role.getNameList().add(otherNameDomain);
					}
				}
				roleName = UserListUtil.toSeparatedRoleNames(user.getRoleList());
			}
			
			// XML
			out.println("<user");
				out.println(" userId=\"" + user.getId() + "\"");
				out.println(" userCode=\"" + StringUtils.xmlEncode(user.getCode()) + "\"");
				out.println(" userName=\"" + StringUtils.xmlEncode(user.getName()) + "\"");
				out.println(" userKana=\"" + StringUtils.xmlEncode(userKana) + "\"");
				out.println(" userMail=\"" + StringUtils.xmlEncode(userMail) + "\"");
				out.println(" groupName=\"" + StringUtils.xmlEncode(groupName) + "\"");
				out.println(" roleName=\"" + StringUtils.xmlEncode(roleName) + "\"");
				out.println(" userDisable=\"" + StringUtils.xmlEncode(userDisable) + "\"");
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