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
	String prmObjId = request.getParameter("objId");
	String prmParentWin = request.getParameter("parentWin");

	//Message
	String message = null;
	Object[] paramId = {
			"objId=" + prmObjId,
			"parentWin=" + prmParentWin
			};
	try
	{
		//Session
		sess = EIMUtils.getSession(request);
		if(sess == null)
		{
			message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			log.warn(AppMessageUtils.makeLogMessage(message));
			return;
		}
		loginUser = (EIMUser)sess.getAttribute("USER");
		
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// 対象文書またはフォルダが選択されていない場合
		if( prmObjId == null || prmObjId == "" ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NODOCANDFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NODOCANDFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		
		// Object
		EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
		if(object == null || !SecurityUtils.authorized(sess, object, sess.getUser(),EIMAccessRole.READ))
		{
			message = EIMResource.getMessage(sess, "EIM.ERROR.INPUT.NODOCANDFOLDER");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.INPUT.NODOCANDFOLDER");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		// PDF または PDF に変換されるドキュメントかをチェック
		if( PublishAddonUtils.isPDFDocument(sess, object) == false ) {
			message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.PDFSIG.SETTING.OUTOFTARGET");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.LOGIC.PDFSIG.SETTING.OUTOFTARGET");
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		// ワークフロー公開処理オブジェクト
		EIMObject wfPubObj = PublishAddonUtils.getWfPubObjFromDocObj(sess, object);
		// PDF 署名オブジェクト取得
		EIMObjectType objTypePDFSig = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_SIGNPDF"));
		EIMObject pdfSigObj  = ObjectUtils.getObjectByTypeAndName(sess, objTypePDFSig, String.valueOf(object.getId()));
		out.println("<setPDFOutput ");
		if( pdfSigObj != null ) {
			/** PDF署名オブジェクトが存在する場合 */
			if( prmParentWin.equalsIgnoreCase("requestApprove") ) {
				
				//承認依頼時の場合にも, そのPDF署名オブジェクトの設定を表示
				//チケット#355によって機能定義書に合うように修正する　by lin.chen at 2010/03/18
				out.println(PublishAddonUtils.getSignAndSetSecurityConfig(sess,pdfSigObj));
/*				
				// 承認依頼時の場合, ワークフロー公開処理設定オブジェクトから設定値をとる
				if( wfPubObj != null ) {
					out.println(PublishAddonUtils.getSignAndSetSecurityConfig(sess,wfPubObj));
				} else {
					out.clear();
					message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
					log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
					return;
				}
*/
			} else {
				// 承認時の場合, そのPDF署名オブジェクトの設定を表示
				out.println(PublishAddonUtils.getSignAndSetSecurityConfig(sess,pdfSigObj));
			}
		} else if( prmParentWin.equalsIgnoreCase("requestApprove") ) {
			/** PDF署名オブジェクトが存在しない 承認依頼の場合 */
			// 承認依頼時の場合, ワークフロー公開処理設定オブジェクトから設定値をとる
			if( wfPubObj != null ) {
				out.println(PublishAddonUtils.getSignAndSetSecurityConfig(sess,wfPubObj));
			} else {
				out.clear();
				message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
				return;
			}
		} else if( prmParentWin.equalsIgnoreCase("actApprove") ) {
			/** PDF署名オブジェクトが存在しない 承認の場合 */
			// PDF 署名オブジェクトがない場合, 承認時ならセキュリティ設定はないものとする
			out.println(" doSignPDFAndSetSecurity=\"false\"");
		} else {
			out.clear();
			message = EIMResource.getMessage(sess, "EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			log.error(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId))	;
			return;
		}
		out.println(">");
		out.println("</setPDFOutput>");
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
		message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
		out.println(AppMessageUtils.makeErrorTagByMessage(message));
		log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
	}
	finally
	{
		try{
			if(sess != null) sess.close();
		}
		catch (Exception se) {
			out.clear();
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			out.println(AppMessageUtils.makeErrorTagByMessage(se.getMessage()));
			log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
		}
	}
	
%>
