package servlet.external;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.DisplayColorUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 外部インタフェース : タグ新規作成
 *
 */
public class CreateTagEx extends HttpServlet
{
	private static final long serialVersionUID = 1L;

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		Log log = LogFactory.getLog(this.getClass().getName());

		//ContentType
		response.setContentType("text/xml; charset=UTF-8");
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");

	    // 出力用PrintWriterを取得
	    PrintWriter out = response.getWriter();

		EIMSession sess = null;
		EIMUser loginUser = null;

		boolean sessPutFlag = false;
		
		//Message
		String message = null;

		//クライアントから受け渡されたパラメタの取得
		String prmUserCode = EIMUtils.getParameter(request, "userCode");
		String prmUserPass = EIMUtils.getParameter(request, "userPass");
		String prmFolderPath = EIMUtils.getParameter(request, "folderPath");
		String prmObjTypeName = EIMUtils.getParameter(request, "objTypeName");
		String prmTagName = EIMUtils.getParameter(request, "tagName");
		String prmProperty = EIMUtils.getParameter(request, "property");
		String prmExpireDate = EIMUtils.getParameter(request, "expireDate");
		//属性名・属性値もUTF-8に変換する
		String prmAttTypeName[] = request.getParameterValues("attTypeName");
		if(prmAttTypeName != null ) {
			for(int ii = 0; ii < prmAttTypeName.length; ii++) {
				prmAttTypeName[ii] = new String(prmAttTypeName[ii].getBytes("iso-8859-1"), "UTF-8");
			}
		}
		String prmAttValue[] = request.getParameterValues("attValue");
		if(prmAttValue != null) {
			for(int ii = 0; ii < prmAttValue.length; ii++) {
				prmAttValue[ii] = new String(prmAttValue[ii].getBytes("iso-8859-1"), "UTF-8");
			}
		}

		//パラメタ配列の作成
		ArrayList paramIdList = new ArrayList();
		paramIdList.add("userCode=" + prmUserCode);
		// パスワードがログに残るのは好ましくない筈
		// paramIdList.add("userPass=" + prmUserPass);
		paramIdList.add("folderPath=" + prmFolderPath);
		paramIdList.add("objTypeName=" + prmObjTypeName);
		paramIdList.add("tagName=" + prmTagName);
		paramIdList.add("property=" + prmProperty);
		paramIdList.add("expireDate=" + prmExpireDate);
		if( prmAttTypeName != null && prmAttValue != null ) {
			for(int k = 0 ; k < Math.min(prmAttTypeName.length, prmAttValue.length) ; k++)
			{
				paramIdList.add("attTypeName[" + k + "]=" + prmAttTypeName[k]);
				paramIdList.add("attValue[" + k + "]=" + prmAttValue[k]);
			}
		}
		Object[] paramId = paramIdList.toArray();

		// Cache
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");

		try
		{
			//パラメタチェック(必須パラメタの指定が無い、あるいはブランク値の場合はエラー)
			String lackedParam = "";
			if( StringUtils.nullToBlank(prmUserCode).equals("") ) {
				lackedParam = "userCode";
			}
			else if( StringUtils.nullToBlank(prmUserPass).equals("") ) {
				lackedParam = "userPass";
			}
			else if( StringUtils.nullToBlank(prmFolderPath).equals("") ) {
				lackedParam = "folderPath";
			}
			else if( StringUtils.nullToBlank(prmObjTypeName).equals("") ) {
				lackedParam = "objTypeName";
			}
			else if( StringUtils.nullToBlank(prmTagName).equals("") ) {
				lackedParam = "tagName";
			}
			if( !lackedParam.equals("") ) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXTIF.NOPARAM", new Object[]{ lackedParam });
				log.warn(AppMessageUtils.makeErrorTagByMessage(message));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			//ログイン
			loginUser = EIMUtils.login(request);
			if( loginUser != null ) {
				request.getSession().setAttribute(EIMSession.USER, loginUser);
			}
			//デフォルト言語を指定
			request.getSession().setAttribute(EIMSession.LANG, EIMConfig.get("MESSAGELANG"));
			//Session
			sess = EIMUtils.getSession(request);
			if(sess == null)
			{
				message = EIMResource.getMessage("EIM.ERROR.SESSIONTIMEOUT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
			
			// ThreadLocalコンテキストにセッションを設定
			if(EIMThreadContext.getEIMSession() == null)
			{
				EIMThreadContext.putEIMSession(sess);
				sessPutFlag = true;
			}
			
			// Set TimezoneOffset
			// ※ EIMログイン画面からのログイン処理では、クライアントから渡してもらっている
			// ※ 外部I/Fの場合はAppサーバのタイムゾーンを使う
			String clTzOffset = String.valueOf((-1)*eim.util.DateUtils.getAPTzOffset());
			sess.setAttribute("clTzOffset", clTzOffset);
			String dbTzOffset = String.valueOf(eim.util.DateUtils.selectDBTzOffset(sess));
			sess.setAttribute("dbTzOffset", dbTzOffset);

			// Windows禁止文字チェック
			AppObjectUtil.checkValidateFName(sess, prmTagName);

			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			//オブジェクトタイプの取得
			EIMObjectType objType = null;
			if ( prmObjTypeName != null ) {
				String tName = prmObjTypeName;
				if(prmObjTypeName.equals(EIMConfig.get("EIM.LABEL.OBJECTTYPE.TAG.GENERAL"))) {
					//「一般タグ」指定の場合は「タグ」としてタグタイプを取得
					tName = EIMConfig.get("EIM.LABEL.OBJECTTYPE.TAG");
				}
				objType = ObjectUtils.getObjectTypeByName(sess, tName);
				if( objType == null ) {
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAGTYPE");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOTAGTYPE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
				// オブジェクトタイプがタグのものかどうかチェック
				else if( !helper.isTypeOfTag(objType) ) {
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXTIF.INVALIDTYPE");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXTIF.INVALIDTYPE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}

			}

			//親オブジェクトの取得
			EIMObject parentObj = AppObjectUtil.getObjListByFullPass(sess, prmFolderPath, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
			if(parentObj == null) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOWSFOL");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOWSFOL");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;

			}
			
			// 作成権限のチェック
			if(!SecurityUtils.authorized(sess, parentObj, sess.getUser(), EIMAccessRole.CREATE))
			{
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}
			
			//パスの取得
			String path = AppObjectUtil.getPath(parentObj);
			if(path == null) {
				// ワークスペースの場合、パス属性の値を保持していない
				path = "/";
			}
			path += parentObj.getName() + "/";

			//親オブジェクトがゴミ箱の下に移動していないかのチェック
			if (AppObjectUtil.isPathInRecycle(path)) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.SELECTFOLDERDELETED");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			//ワークフロー付きフォルダ配下には作成できない
			if (parentObj.getStatus() != null) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTCREATE.UNDERWF.TAG");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTCREATE.UNDERWF.TAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			//指定されたタグオブジェクトの作成
			EIMObject object = ObjectUtils.createObject(sess, objType, prmTagName);
			if(object == null)
			{
				sess.rollback();
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.FAILMAKEOBJ");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.FAILMAKEOBJ");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			//親オブジェクトとのリレーションを作成
			EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
			try{
				RelationUtils.createRelation(sess, relType, parentObj, object,  EIMConstant.DEPU_CHECK_NAME_REV);
			}
			catch(EIMException eime) {
				sess.rollback();
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
				message = eime.getMessage();
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			//パスを設定
			AppObjectUtil.setPath(sess, object, path);

			EIMAttributeType attType = null;
			//プロパティを設定
			if( prmProperty != null )
			{
				attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PROP"));
				ObjectAttributeUtils.setAttribute(sess, object, attType, prmProperty);
			}

			//作成者を設定
			if(loginUser != null)
			{
				attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE"));
				ObjectAttributeUtils.setAttribute(sess, object, attType, loginUser.getId());
			}

			//有効期限を設定
			if(prmExpireDate != null && prmExpireDate.length() > 0)
			{
				Date expireDate = DateUtils.editExpirationDate(sess,
						StringUtils.getDateFromString(prmExpireDate, EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
				attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
				ObjectAttributeUtils.setAttribute(sess, object, attType, expireDate);
			}

			String[] nonTableAttributes = AppConstant.SYSTEM_ATTRIBUTE_DEFNAME;
			//その他の指定属性を設定
			if( prmAttTypeName != null && prmAttValue != null ) {
				//オブジェクトタイプが保持する属性を取得
				List existAttrList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
				for(int ii = 0; ii < Math.min(prmAttTypeName.length, prmAttValue.length); ii++) {
					attType = AttributeUtils.getAttributeTypeByName(sess, prmAttTypeName[ii]);
					// システム属性に該当するかどうかチェックする
					if(attType != null) {
						for( int jj = 0; jj < nonTableAttributes.length; jj++ ) {
							// システム属性の場合はエラー終了
							if( attType.getDefName().equals(nonTableAttributes[jj]) ) {
								sess.rollback();
								message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANT.SET.SYSATTR");
								log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
								message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANT.SET.SYSATTR");
								out.println(AppMessageUtils.makeErrorTagByMessage(message));
								return;
							}
						}

						int jj = 0;
						for(; jj < existAttrList.size(); jj++) {
							if( attType.getDefName().equals(((EIMAttributeType)existAttrList.get(jj)).getDefName())) {
								break;
							}
						}
						if(jj >= existAttrList.size()) {
							// 作成するオブジェクトが保持しない属性の場合はエラー終了
							sess.rollback();
							message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
							log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
							message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
							out.println(AppMessageUtils.makeErrorTagByMessage(message));
							return;
						}

						// 対象オブジェクトの属性値に入力値を設定
						// ※ 親オブジェクトの下位引継属性を指定してしまった場合は、次の処理で上書きされる
						//    (上位からの属性値が優先)
						boolean existFlag = false;
						switch (attType.getValueType().getId()) {
							// 文字列・テキスト型の場合
							case EIMValueType.STRING:
							case EIMValueType.TEXT:
								// マスタ値の存在チェック
								existFlag = AppObjectUtil.doesExistAttrValueInMasterValues(sess, attType, prmAttValue[ii]);
								if(existFlag == false) {
									sess.rollback();
									message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRIBUTE.VALUE.NOT.REGIST");
									log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
									message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUE.NOT.REGIST");
									out.println(AppMessageUtils.makeErrorTagByMessage(message));
									return;
								}

								if( attType.isMultiple() ) {	// 複数値属性の場合
									String strArray[] = { prmAttValue[ii] };
									ObjectAttributeUtils.setAttribute(sess, object, attType, strArray);
								}
								else {
									ObjectAttributeUtils.setAttribute(sess, object, attType, prmAttValue[ii]);
								}
								break;
							// 数値型の場合
							case EIMValueType.INTEGER:
								// マスタ値の存在チェック
								existFlag = AppObjectUtil.doesExistAttrValueInMasterValues(sess, attType, prmAttValue[ii]);
								if(existFlag == false) {
									sess.rollback();
									message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRIBUTE.VALUE.NOT.REGIST");
									log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
									message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUE.NOT.REGIST");
									out.println(AppMessageUtils.makeErrorTagByMessage(message));
									return;
								}

								int iVal = Integer.parseInt(prmAttValue[ii]);
								if( attType.isMultiple() ) {	// 複数値属性の場合
									long nArray[] = { iVal };
									ObjectAttributeUtils.setAttribute(sess, object, attType, TypeConvertUtils.convertToBuildTypeArray(nArray));
								}
								else {
									ObjectAttributeUtils.setAttribute(sess, object, attType, iVal);
								}
								break;
								// double数値型の場合
							case EIMValueType.DOUBLE:
								// マスタ値の存在チェック
								existFlag = AppObjectUtil.doesExistAttrValueInMasterValues(sess, attType, prmAttValue[ii]);
								if(existFlag == false) {
									sess.rollback();
									message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRIBUTE.VALUE.NOT.REGIST");
									log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
									message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUE.NOT.REGIST");
									out.println(AppMessageUtils.makeErrorTagByMessage(message));
									return;
								}

								double dlVal = Double.parseDouble(prmAttValue[ii]);
								if( attType.isMultiple() ) {	// 複数値属性の場合
									double nArray[] = { dlVal };
									ObjectAttributeUtils.setAttribute(sess, object, attType, nArray);
								}
								else {
									ObjectAttributeUtils.setAttribute(sess, object, attType, dlVal);
								}
								break;
							// 日付型の場合
							case EIMValueType.DATE:
								// マスタ値の存在チェック
								existFlag = AppObjectUtil.doesExistAttrValueInMasterValues(sess, attType, prmAttValue[ii]);
								if(existFlag == false) {
									sess.rollback();
									message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTRIBUTE.VALUE.NOT.REGIST");
									log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
									message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTRIBUTE.VALUE.NOT.REGIST");
									out.println(AppMessageUtils.makeErrorTagByMessage(message));
									return;
								}

								Date dVal = DateUtils.editExpirationDate(sess, StringUtils.getDateFromString(prmAttValue[ii],
										EIMResource.getMessage(sess, "EIM.FORMAT.DATE")));
								if( attType.isMultiple() ) {	// 複数値属性の場合
									Date dArray[] = { dVal };
									ObjectAttributeUtils.setAttribute(sess, object, attType, dArray);
								}
								else {
									ObjectAttributeUtils.setAttribute(sess, object, attType, dVal);
								}
								break;
							default:
								break;
						}

						if(attType.isMultiple() == false) {
							//リスト値表示色オブジェクト作成
							DisplayColorUtil.createDisplayColorFromExternalIF(
									sess, request, String.valueOf(object.getId()), prmAttValue[ii], attType);
						}
					}
					else {	// 属性タイプが存在しないものだった場合
						sess.rollback();
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
						log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.ATTTYPE.NOTFOUND");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						return;
					}
				}
			}

			//セキュリティを設定
			EIMSecurity sec = parentObj.getSecurity();
			if(sec != null)
			{
				SecurityUtils.setSecurity(sess, object, sec);
			}
			
			// SearchFramework 検索FW更新通知 対象：タグオブジェクト、登録先のフォルダorワークスペース
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CREATE_TAG");
			AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj, 
					"SEARCHFW_CREATE_TAG_PARENT_FOLDER", "SEARCHFW_CREATE_TAG_PARENT_WORKSPACE", null);
			
			//Access
			AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.INITIALREGIST");

			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.CREATE_TAG_EXCOMMAND,
					EIMConstant.TARGET_CREATE, EIMConstant.OBJECT_TYPE, object,
					null, null, null, path);

			//処理成功時のXMLを出力
			out.println("<ok");
				out.println(" objId=\"" + object.getId() + "\"");
				out.println(" parentObjId=\"" + parentObj.getId() + "\"");
			out.println(">");
			out.println("</ok>");

			//Commit
			sess.commit();
		}
		catch(EIMException eime)
		{
			message = eime.getMessage();
			if(loginUser != null)
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), eime.getMessage(), paramId), eime);
			else
				log.warn(AppMessageUtils.makeLogMessage(0, eime.getMessage(), paramId), eime);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));

			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				message = se.getMessage();
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
			}
		}
		catch(Exception e)
		{
			message = e.getMessage();
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			out.println(AppMessageUtils.makeErrorTagByMessage(message));
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				message = se.getMessage();
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
			}
		}
		finally
		{
			try{
				
				if(sessPutFlag) {
					EIMThreadContext.removeEIMSession();
					sessPutFlag = false;
				}
				if(sess != null){
					sess.close();
				}
				//Invalid Session
				request.getSession().invalidate();
			}
			catch (Exception se) {
				message = se.getMessage();
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
			}
		}
	}

	protected void doGet(	HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		doPost(request, response);
	}

}
