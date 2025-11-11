<%@ page contentType="text/plain; charset=UTF-8" %>
<%@ page import = "eim.net.*" %>
<%@ page import = "eim.bo.*" %>
<%@ page import = "eim.util.*" %>
<%@ page import = "java.util.*" %>
<%@ page import = "org.apache.commons.logging.*" %>
<%@ page import = "common.bo.*" %>
<%@ page import = "common.util.*" %>

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
	Object paramId = null;
	
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

		//Parameter
		String prmObjIds[] = request.getParameterValues("objId");
		
		if( prmObjIds == null ) {
			out.println("<ok></ok>");
			return;			
		}
		
		//Helper
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

		//Object Type Reply
		EIMObjectType objTypeReply = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_RECEIVE"));

		List objIdList = new ArrayList();
		for(int i = 0; i < prmObjIds.length; i++ ) {
			objIdList.add(prmObjIds[i]);
		}

		//Send Mail
		try {
			SendMailRecursive.sendReplyMailRecursive(sess, user, objTypeReply, objIdList, true, helper);
		}
		catch( EIMException eime) {

			out.println("<ok></ok>");
			sess.close();
			return;			
		}
				
		//Commit
		sess.commit();
		out.println("<ok></ok>");
	}
	catch(EIMException eime)
	{
		out.clear();
		message = eime.getMessage();
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.warn(AppMessageUtils.makeLogMessage(user.getId(), eime.getMessage(), new Object[]{paramId}), eime);
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
		if(jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.getTransactionContext() != null)
		{
			jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext.removeTransactionContext();
		}
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
<%!
/**
 * 再帰的に受信確認メールを送信するクラス
 */
static class SendMailRecursive {
	
	/**
	 * 渡されてきたEIMObjectのリストからドキュメント・WF付フォルダの受信確認メールを
	 * 送信する再帰処理
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param user EIMUserインスタンス
	 * @param objType 受信確認オブジェクトのEIMObjectTypeインスタンス
	 * @param prmObjIds EIMObjectのリスト
	 * @param isFolTagRecursive リスト中にフォルダ・タグがある場合に再帰処理を行うかどうか
	 * @param helper AppObjectConditionHelperインスタンス
	 *
	 * @return ドキュメントの合計数
	 * @throws Exception 
	 */
	public static int sendReplyMailRecursive(EIMSession sess, EIMUser user, EIMObjectType objType, List prmObjIds, boolean isFolTagRecursive, AppObjectConditionHelper helper) throws Exception {

		int cnt = 0;
		
		Iterator i = prmObjIds.iterator();
		for (;i.hasNext();) {
			String objId = (String)i.next();

			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(objId));
			if(object == null)	{
				continue;
			}

			// オブジェクトがドキュメントの場合の処理
			if( helper.isTypeOfDocument(object.getType()) ) {
				//メール送信処理
				sendMail(sess, user, objType, object);
				cnt++;
			}				
			// オブジェクトがフォルダの場合の処理
			else if( helper.isTypeOfFolder(object.getType()) ) {
				
				if(isFolTagRecursive) {	//フォルダに対して再帰処理を行う場合
					//ドキュメントリンクを含むオブジェクトを取得する
					HashMap isDocLinkMapTmp = new HashMap();
					List childObjs = helper.getChildObjectsWithDocLinkInAccessibleStatus(object, isDocLinkMapTmp);
					if( childObjs != null ) {
						List childOids = new ArrayList();
						for(Iterator jj = childObjs.iterator(); jj.hasNext(); ) {
							EIMObject objTmp = (EIMObject)jj.next();
							childOids.add(String.valueOf( objTmp.getId() ));
						}
						//再帰処理
						cnt += sendReplyMailRecursive(sess, user, objType, childOids, true, helper);
					}
				}
			}
			// オブジェクトがタグの場合の処理
			else if( helper.isTypeOfTag(object.getType()) ) {
				
				if(isFolTagRecursive) {	//タグに対して再帰処理を行う場合
					// タグ配下のオブジェクトを全て取得する
					TagTreeItem tagItems = TagUtil.getTagTreeWithChild(sess, object);
					if( tagItems != null && tagItems.getTreeItemList() != null ) {
						
						//再帰処理
						cnt += sendMailUnderTagRecursive(sess, user, objType, tagItems.getTreeItemList(), helper);
					}
				}
			}
		}
		
		return cnt;
	}

	/**
	 * 渡されてきたTagTreeItemのリストからドキュメント・WF付フォルダの受信確認メールを
	 * 送信する再帰処理
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param user EIMUserインスタンス
	 * @param objType 受信確認オブジェクトのEIMObjectTypeインスタンス
	 * @param prmObjIds TagTreeItemのリスト
	 * @param helper AppObjectConditionHelperインスタンス
	 *
	 * @return ドキュメントの合計数
	 * @throws Exception 
	 */
	private static int sendMailUnderTagRecursive(EIMSession sess, EIMUser user, EIMObjectType objType, List tagChildObjs, AppObjectConditionHelper helper) throws Exception {

		int cnt = 0;

		Iterator i = tagChildObjs.iterator();
		for (; i.hasNext(); ) {
			TagTreeItem childItem = (TagTreeItem)i.next();
			EIMObject object = childItem.getEimObject();
			// オブジェクトがドキュメントの場合の処理
			if( helper.isTypeOfDocument(object.getType()) ) {
				//メール送信処理
				sendMail(sess, user, objType, object);
				cnt++;
			}				
			// フォルダ・タグの場合はTagTreeItem._treeItemListについて再帰処理
			else if( (helper.isTypeOfFolder(object.getType()) || helper.isTypeOfTag(object.getType())) &&
					childItem.getTreeItemList() != null) {				
								
				//再帰処理
				cnt += sendMailUnderTagRecursive(sess, user, objType, childItem.getTreeItemList(), helper);
			}			
		}
		
		return cnt;
	}
	
	/**
	 * メールを送信する処理
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param user EIMUserインスタンス
	 * @param objType 受信確認オブジェクトのEIMObjectTypeインスタンス
	 * @param object 受信確認メール送信対象のオブジェクト
	 * @param helper AppObjectConditionHelperインスタンス
	 */
	private static void sendMail(EIMSession sess, EIMUser user, EIMObjectType objType, EIMObject object) throws Exception {

		
		//Reply Object
		EIMObject replyObj = ObjectUtils.getObjectByTypeAndName(sess, objType, object.getId() + "." + user.getId());
		//受信確認オブジェクトが存在しなければなにもしない
		if(replyObj == null)
		{
			return;
		}
		
		//受信確認メールを送信
		MailUtil.execute(sess, object, "IMPL.TYPE.RECEPTION");

		//Delete
		ObjectUtils.deleteObject(sess, replyObj);
	}
}
%>
