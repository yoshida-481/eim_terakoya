<%@ page contentType="text/xml; charset=UTF-8"%>
<%@ page import="eim.bo.*"%>
<%@ page import="eim.net.*"%>
<%@ page import="eim.util.*"%>
<%@ page import="java.util.*"%>

<%@ page import="org.apache.commons.logging.*"%>
<%@ page import="common.util.*"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.ApplicationContextLoader" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.context.TransactionContext" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.enumeration.ConnectionModeEnum" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.common.exception.EIMException"%>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.criteria.SecurityCriteria" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.service.SecurityService" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.business.domain.entity.SecurityDomain" %>
<%@ page import = "jp.co.ctc_g.eim.framework2.integration.dao.impl.internal.ConvertUtils" %>

<%

class StatusSecurityTreeView{
	public StatusSecurityTreeView()
	{
	}
	public void show(EIMSession sess,StringBuffer sb,EIMSecurity sec) throws Exception{
		//XML(Only Writing)

		// デフォルトセキュリティのノード作成
		sb.append("	<security")
		  .append(" label=\"")
		  .append(StringUtils.xmlEncode(sec.getName()))
		  .append("\"")
		  .append(" isBranch=\"");
		// isBranchの為に子供の存在判定
		sb.append("false\"");

		sb.append(" secId=\"")
		  .append(sec.getId())
		  .append("\"")
		  .append("/>\n");

	}
}
	// Error Logging
	Log log = LogFactory.getLog(this.getClass().getName());

	//ContentType
	response.setContentType("text/xml; charset=UTF-8");
	response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	//Session
	EIMSession sess = null;
	EIMUser user = null;

	//	Parameter
	String prmObjId = request.getParameter("objId");
	String prmInitFlag = EIMUtils.getParameter(request, "initFlag");
	String prmSecurityName = EIMUtils.getParameter(request, "securityName");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"initFlag=" + prmInitFlag,
			"securityName=" + prmSecurityName
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
		user = (EIMUser)sess.getAttribute("USER");

		int securityListCnt = 0;	// セキュリティ数
		boolean item0Flag = false;
		int searchLimitMax = -1;
		// 次の条件を全て満たす場合に、セキュリティの空リストを返却する。
		// ・SECURITY_DISPLAY_POLICYが"SEARCH"
		// ・prmInitFlagが"true"
		// ・prmSecurityNameが空の場合
		String securityDisplayParam = EIMConfig.get("SECURITY_DISPLAY_POLICY");
		if(securityDisplayParam!=null && securityDisplayParam.equals("ALL")) {
			// 全件表示
		} else if(securityDisplayParam!=null && securityDisplayParam.equals("SEARCH")) {
			// 検索時表示
			if(prmInitFlag.equals("true")){
				item0Flag = true;
			}
		} else {
			// エラー
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"SECURITY_DISPLAY_POLICY"});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"SECURITY_DISPLAY_POLICY"});
			log.error(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		// 検索結果上限を取得する
		String securityLimitParam = EIMConfig.get("SEARCH_SECURITY_RESULT_MAX");
		if(securityLimitParam!=null){
			searchLimitMax = Integer.parseInt(securityLimitParam);
		} else {
			// エラー
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"SEARCH_SECURITY_RESULT_MAX"});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.CONFIG.SETTING.ERROR", new Object[]{"SEARCH_SECURITY_RESULT_MAX"});
			log.error(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}

		//Root Node
		StringBuffer sb = new StringBuffer();
		sb.append("<secList>\n");

		// item0Flagがtrueの場合は0件返却
		if(!item0Flag) {
			String newSecurityName = prmSecurityName.trim();

			StatusSecurityTreeView sst = new StatusSecurityTreeView();

			//定義名称のあいまい検索を行うためV5APIを使用
			if(EIMThreadContext.getTransactionContext() != null)
			{
				EIMThreadContext.removeTransactionContext();
			}

			TransactionContext context = new TransactionContext(ConnectionModeEnum.DECLARATIVE);
			EIMThreadContext.putTransactionContext(context);
			context.setLangId(sess.getLangId());
			context.setDBConnection(sess.getDBConnection());
			context.setUser(ConvertUtils.toUserDomain(user));

			SecurityService securityService = (SecurityService)ApplicationContextLoader.getApplicationContext().getBean("securityService2");

			if(prmObjId != null)	//Update Security
			{
				//		Object
				EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));

				AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

				if( helper.isTypeOfRecycle(object.getType()) )
				{
					// ごみ箱の場合はなにもしない。
				}
				else if(SecurityUtils.authorized(sess,object,user,EIMAccessRole.UPDATE)		// <-- 対象のObjectに対して更新権限があるか
							&&(helper.isTypeOfFolder(object.getType())						// <-- ＆＆ タイプがフォルダまたはWSか
								|| helper.isTypeOfWorkspace(object.getType())))
					//Writing
				{
					//========================================
					// WS使用可能セキュリティ対応(2012.02.22)
					//========================================

					//選択しているワークスペースの情報を取得
					EIMObject workspaceObj = null;
					if(helper.isTypeOfWorkspace(object.getType())){
						workspaceObj = object;
					}
					else{
						// フォルダを選択している場合は、『所属ワークスペース』属性から取得
						EIMAttribute wsAttr = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_BELONGING_WS"));
						if(wsAttr != null){
							EIMObject belongWs = ObjectUtils.getObjectById(sess, wsAttr.getInt());
							workspaceObj = belongWs;
						}else{

							// 属性が存在しない場合は、所属しているワークスペースを取得しにいく
							List<EIMObject> parentObjList = AppObjectUtil.getParentEIMObjectRecurrently(sess, object, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
							// 再帰的に取得しているので後ろから取得
							for(int i = parentObjList.size()-1; i >= 0; i--){
								EIMObject obj = parentObjList.get(i);
								if(helper.isTypeOfWorkspace(obj.getType())){
									workspaceObj = obj;
									break;
								}
							}
						}
					}

					if(workspaceObj != null){

						List<EIMSecurity> secList = null;
						//使用可能セキュリティ絞込フラグのチェック
						long limitSecFlag = AppObjectUtil.getIntAttr(sess, workspaceObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LIMIT_SECURITY_FLAG"), Integer.MIN_VALUE);
						if(limitSecFlag == Integer.MIN_VALUE || limitSecFlag == AppConstant.FLAG_OFF){
							// 属性なし、または絞り込まない
							// Security List
							// ネームスペースの前方一致で一覧取得
							String defSearchName = NamespaceUtil.concatenate(EIMConfig.get("APP_DOC_NAMESPACE"), null) + "*";
							SecurityCriteria securityCriteria = new SecurityCriteria();
							securityCriteria.setDefinitionName(defSearchName);
							if(newSecurityName != null && !newSecurityName.equals(""))
							{
								// 部分一致の為、*追加
								newSecurityName = "*" + newSecurityName + "*";
							}
							securityCriteria.setName(newSecurityName);

							List<SecurityDomain> securityDomainList = securityService.getList(securityCriteria);

							// ドキュメント管理の場合はネームスペース付きを除外する
							String excludeNameSpaceCSV = EIMConfig.get("NAME_SPACE_TO_EXCLUDE_IN_DOCUMENT");
							if (excludeNameSpaceCSV != null) {
								String[] excludeNameSpaceArray = excludeNameSpaceCSV.split(",");

								List<SecurityDomain> filterdSecurityDomainList = new ArrayList<>();
								for (SecurityDomain securityDomain : securityDomainList) {

									boolean isDocumentNameSpace = true;
									for (int i = 0; i < excludeNameSpaceArray.length; i++) {

										String excludeNameSpace = excludeNameSpaceArray[i].trim();
										if (securityDomain.getDefinitionName().indexOf(excludeNameSpace) == 0) {
											isDocumentNameSpace = false;
										}
									}
									if (isDocumentNameSpace) {
										filterdSecurityDomainList.add(securityDomain);
									}
								}
								securityDomainList = filterdSecurityDomainList;
							}

							// 多言語名称でソートを実装
							securityDomainList = AppObjectUtil.getStrSortedList(securityDomainList, "getName", true);

							// 後続の処理をV4系に合わせるためコンバートをかける
							secList = new ArrayList<EIMSecurity>();
							if(securityDomainList != null){
								for(SecurityDomain securityDomain : securityDomainList){
									secList.add(ConvertUtils.toEIMSecurity(securityDomain));
								}
							}
						}
						else{

							secList = new ArrayList();
							//EIMSecurityIDのSet(重複禁止)
							HashSet<Long> filterSet = new HashSet();

							//使用可能セキュリティを絞り込む
							EIMAttribute selectableSecAttr = workspaceObj.getAttribute(EIMConfig.get("ATTR_NAME_WORKSPACE_SELECTABLE_SECURITY"));
							//使用可能セキュリティ＋現在のセキュリティから
							long[] secIdList = TypeConvertUtils.convertToLongArray(selectableSecAttr.getInts());
							for(long id : secIdList){
								EIMSecurity sec = SecurityUtils.getSecurityById(sess, id);
								if(sec != null){
									filterSet.add((long)sec.getId());
								}
							}
							if(object.getSecurity() != null){
								filterSet.add((long)object.getSecurity().getId());
							}

							// 全てのセキュリティから絞込み
							// Security List
							// ネームスペースの前方一致で一覧取得
							String defSearchName = NamespaceUtil.concatenate(EIMConfig.get("APP_DOC_NAMESPACE"), null) + "*";
							SecurityCriteria securityCriteria = new SecurityCriteria();
							securityCriteria.setDefinitionName(defSearchName);
							if(newSecurityName != null && !newSecurityName.equals(""))
							{
								// 部分一致の為、*追加
								newSecurityName = "*" + newSecurityName + "*";
							}
							securityCriteria.setName(newSecurityName);
							List<SecurityDomain> securityDomainList = securityService.getList(securityCriteria);

							// ドキュメント管理の場合はネームスペース付きを除外する
							String excludeNameSpaceCSV = EIMConfig.get("NAME_SPACE_TO_EXCLUDE_IN_DOCUMENT");
							if (excludeNameSpaceCSV != null) {
								String[] excludeNameSpaceArray = excludeNameSpaceCSV.split(",");

								List<SecurityDomain> filterdSecurityDomainList = new ArrayList<>();
								for (SecurityDomain securityDomain : securityDomainList) {

									boolean isDocumentNameSpace = true;
									for (int i = 0; i < excludeNameSpaceArray.length; i++) {

										String excludeNameSpace = excludeNameSpaceArray[i].trim();
										if (securityDomain.getDefinitionName().indexOf(excludeNameSpace) == 0) {
											isDocumentNameSpace = false;
										}
									}
									if (isDocumentNameSpace) {
										filterdSecurityDomainList.add(securityDomain);
									}
								}
								securityDomainList = filterdSecurityDomainList;
							}

							// 多言語名称でソートを実装
							securityDomainList = AppObjectUtil.getStrSortedList(securityDomainList, "getName", true);

							// 後続の処理をV4系に合わせるためコンバートをかける
							List<EIMSecurity> allList = new ArrayList<EIMSecurity>();
							if(securityDomainList != null){
								for(SecurityDomain securityDomain : securityDomainList){
									allList.add(ConvertUtils.toEIMSecurity(securityDomain));
								}
							}

							for(int i=0; i < allList.size(); i++){
								EIMSecurity sec = allList.get(i);
								if(filterSet.contains((long)sec.getId())){
									secList.add(sec);
								}
							}
						}

						//アクセスエントリーにログインユーザの書き込み権限が設定されているセキュリティのみに絞り込む。
						for(int i = 0; i < secList.size(); i++)
						{
							//Security
							EIMSecurity sec = (EIMSecurity)secList.get(i);
							SecurityUtils.setSecurity(sess, object, sec);
							if(SecurityUtils.authorized(sess,object, sess.getUser(), EIMAccessRole.UPDATE))
							{
								sst.show(sess,sb,sec);
								securityListCnt++;
							}
						}
					}
				}
				else //Reading
				{
					EIMSecurity sec = object.getSecurity();
					sst.show(sess,sb,sec);
					securityListCnt++;
				}
			}
			else //Create Workspace
			{
	//			Security List
				//ネームスペースの前方一致で一覧取得
				String defSearchName = NamespaceUtil.concatenate(EIMConfig.get("APP_DOC_NAMESPACE"), null) + "*";
				SecurityCriteria securityCriteria = new SecurityCriteria();
				securityCriteria.setDefinitionName(defSearchName);
				if(newSecurityName != null && !newSecurityName.equals(""))
				{
					// 部分一致の為、*追加
					newSecurityName = "*" + newSecurityName + "*";
				}
				securityCriteria.setName(newSecurityName);

				List<SecurityDomain> securityDomainList = securityService.getList(securityCriteria);

				// ドキュメント管理の場合はネームスペース付きを除外する
				String excludeNameSpaceCSV = EIMConfig.get("NAME_SPACE_TO_EXCLUDE_IN_DOCUMENT");
				if (excludeNameSpaceCSV != null) {
					String[] excludeNameSpaceArray = excludeNameSpaceCSV.split(",");

					List<SecurityDomain> filterdSecurityDomainList = new ArrayList<>();
					for (SecurityDomain securityDomain : securityDomainList) {

						boolean isDocumentNameSpace = true;
						for (int i = 0; i < excludeNameSpaceArray.length; i++) {

							String excludeNameSpace = excludeNameSpaceArray[i].trim();
							if (securityDomain.getDefinitionName().indexOf(excludeNameSpace) == 0) {
								isDocumentNameSpace = false;
							}
						}
						if (isDocumentNameSpace) {
							filterdSecurityDomainList.add(securityDomain);
						}
					}
					securityDomainList = filterdSecurityDomainList;
				}

				//他言語名称でソートを実装
				securityDomainList = AppObjectUtil.getStrSortedList(securityDomainList, "getName", true);

				//後続の処理をV4系に合わせるためコンバートをかける
				List<EIMSecurity> secList = new ArrayList<EIMSecurity>();
				if(securityDomainList != null){
					for(SecurityDomain securityDomain : securityDomainList){
						secList.add(ConvertUtils.toEIMSecurity(securityDomain));
					}
				}

				EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, "ワークスペース");

				EIMObject object = ObjectUtils.createObject(sess, objType, "tempWorkSpace");
				for(int i = 0; i < secList.size(); i++)
				{
					//Security
					EIMSecurity sec = (EIMSecurity)secList.get(i);

					SecurityUtils.setSecurity(sess, object, sec);
					//XML
					if(SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE))
					{
						sst.show(sess,sb,sec);
						securityListCnt++;
					}
				}
			}
		}

		if(searchLimitMax!=-1 && (securityListCnt > searchLimitMax)) {
			// セキュリティの数がオーバしていた場合エラーにする。
			// エラー
			out.clear();
			message = EIMResource.getMessageValue(sess, "EIM.ERROR.LOGIC.SEARCH.RESULT.LIMIT.OVER", new Object[]{securityListCnt,searchLimitMax});
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessageValue("EIM.ERROR.LOGIC.SEARCH.RESULT.LIMIT.OVER", new Object[]{securityListCnt,searchLimitMax});
			log.warn(AppMessageUtils.makeLogMessage(user.getId(), message, paramId));
			return;
		}


		//End Root Node
		sb.append("</secList>\n");
		//output XML
		out.println(sb.toString());
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
			if(EIMThreadContext.getTransactionContext() != null){
				EIMThreadContext.removeTransactionContext();
			}

			if(sess != null){
				sess.rollback();
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
