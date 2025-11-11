package servlet.external;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AdminAuthUtil;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppSecurityUtils;
import common.util.FormatUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.UserUtils;

/**
 * 外部インタフェース : 属性情報取得
 *
 */
public class GetAttributesEx extends HttpServlet
{
	private static final long serialVersionUID = 1L;

	private static final String TARGET_TYPE_DOCUMENT	= "document";
	private static final String TARGET_TYPE_FOLDER	= "folder";
	private static final String TARGET_TYPE_TAG		= "tag";

	//システム属性のうち、本I/Fで出力してよい属性の配列(config.nativeの定義名)
	private static final String[] readOnlySystemAttr = {
									"ATTR_NAME_DOCUMENT_SIGN_ENC_VER"
								};

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

		//Message
		String message = null;

		//クライアントから受け渡されたパラメタの取得
		String prmUserCode = EIMUtils.getParameter(request, "userCode");
		String prmUserPass = EIMUtils.getParameter(request, "userPass");
		String prmTargetType = EIMUtils.getParameter(request, "targetType");
		String prmTargetPath = EIMUtils.getParameter(request, "targetPath");

		//パラメタ配列の作成
		ArrayList paramIdList = new ArrayList();
		paramIdList.add("userCode=" + prmUserCode);
		// パスワードがログに残るのは好ましくない筈
		// paramIdList.add("userPass=" + prmUserPass);
		paramIdList.add("targetType=" + prmTargetType);
		paramIdList.add("targetPath=" + prmTargetPath);
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
			else if( StringUtils.nullToBlank(prmTargetType).equals("") ) {
				lackedParam = "targetType";
			}
			else if( StringUtils.nullToBlank(prmTargetPath).equals("") ) {
				lackedParam = "targetPath";
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
			// Set TimezoneOffset
			// ※ EIMログイン画面からのログイン処理では、クライアントから渡してもらっている
			// ※ 外部I/Fの場合はAppサーバのタイムゾーンを使う
			String clTzOffset = String.valueOf((-1)*eim.util.DateUtils.getAPTzOffset());
			sess.setAttribute("clTzOffset", clTzOffset);
			String dbTzOffset = String.valueOf(eim.util.DateUtils.selectDBTzOffset(sess));
			sess.setAttribute("dbTzOffset", dbTzOffset);

			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			// 対象オブジェクトの取得
			String strType = "";
			if( prmTargetType.equals(TARGET_TYPE_DOCUMENT) )
				strType = EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT");
			else if( prmTargetType.equals(TARGET_TYPE_FOLDER) )
				strType = EIMConfig.get("OBJECT_TYPE_NAME_FOLDER");
			else if( prmTargetType.equals(TARGET_TYPE_TAG) )
				strType = EIMConfig.get("OBJECT_TYPE_NAME_TAG");

			EIMObject object = AppObjectUtil.getObjListByFullPass(sess, prmTargetPath, strType);
			if (object == null)
			{
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCORFOLORTAG");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			// 対象オブジェクトの参照権限のチェック(
			// ※ 検索できている時点で権限がないことは考えられないが・・・
			try {
				helper.checkAccessibleStatusSelf(object, true);
			} catch (EIMException e) {
				if (!e.getMessageKey().equals("EIM.ERROR.LOGIC.NOACCESS")) throw e;
				message = e.getMessage();
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			// 対象オブジェクトのオブジェクトタイプの取得
			EIMObjectType objType = ObjectUtils.getObjectTypeById(sess, object.getType().getId());
			if( objType == null ) {
				String messageKey = "";
				if( prmTargetType.equals(TARGET_TYPE_DOCUMENT) )
					messageKey = "EIM.ERROR.LOGIC.NODOCUMENTTYPE";
				else if( prmTargetType.equals(TARGET_TYPE_FOLDER) )
					messageKey = "EIM.ERROR.LOGIC.NOFOLDERTYPE";
				else if( prmTargetType.equals(TARGET_TYPE_TAG) )
					messageKey = "EIM.ERROR.LOGIC.NOTAGTYPE";

				message = EIMResource.getMessage(messageKey);
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, messageKey);
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			// 属性タイプのリストを取得
			List attTypeList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
			if( attTypeList.size() > 1 )	//IDでソート
				attTypeList = AppObjectUtil.getIntSortedList(attTypeList, "getId", true);

			// オブジェクト固有のパラメタ値を出力する
			EIMFile file = null;
			long fSize = 0;
			/*
			 * 有効期限切れ判定(ステータスタイプ名の表示に影響するので先に判定する)
			 */
			boolean expiration = false;	//有効期限切れフラグ
			EIMAttribute expirationDate = object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_EFFECT_DATE"));
			if(expirationDate != null)
			{
				expiration = DateUtils.judgeExpirationDate(sess, expirationDate.getDate());
			}
			//XML
			out.println("<objList>");
				out.println(" <object");
					out.println("  objId=\"" + object.getId() + "\"");
					out.println("  objTypeId=\"" + object.getType().getId() + "\"");
					//オブジェクトタイプ名については以下の置換を行う
					//(1)「ドキュメント」→「一般ドキュメント」
					//(2)「フォルダ」→「一般フォルダ」
					//(3)「タグ」→「一般タグ」
					String objTypeName = "";
					if(object.getType().getDefaultName().equals(EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT")))
						objTypeName = EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL");
					else if(object.getType().getDefaultName().equals(EIMConfig.get("OBJECT_TYPE_NAME_FOLDER")))
						objTypeName = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.FOLDER.GENERAL");
					else if(object.getType().getDefaultName().equals(EIMConfig.get("OBJECT_TYPE_NAME_TAG")))
						objTypeName = EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.TAG.GENERAL");
					else
						objTypeName = object.getType().getName();
					out.println("  objTypeName=\"" + StringUtils.xmlEncode(objTypeName) + "\"");
					out.println("  objName=\"" + StringUtils.xmlEncode(object.getName()) + "\"");
					out.println("  rev=\"" + object.getRev() + "\"");
					out.println("  latest=\"" + object.getLatest() + "\"");
					// Create User Name
					String createUserName = "";
					if ( helper.isTypeOfDocument(objType) || helper.isTypeOfTag(objType) ) {
						if (object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE")) != null) {
							createUserName = UserUtils.getUserById(sess, object.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE")).getInt()).getName();
						}
					}
					else {
						createUserName = object.getCreateUser().getName();
					}
					out.println("  createUserName=\"" + StringUtils.xmlEncode(createUserName) + "\"");

					out.println("  createDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getCreateDate()) + "\"");
					out.println("  modifyUserName=\"" + StringUtils.xmlEncode(object.getModifyUser().getName()) + "\"");
					out.println("  modifyDate=\"" + DateUtils.getDBTzToCLTzDate(sess, object.getModifyDate()) + "\"");
					// Size
					if ( helper.isTypeOfDocument(objType) ) {
						file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
						if( file != null )
							fSize = file.getSize();
						out.println("  fileSize=\"" + fSize + "\"");
					}

					out.println("  url=\"" + StringUtils.xmlEncode(EIMConfig.get("DOCUMENT_URL") +  EIMConfig.get("QUERY_STRING")) + "objId=" + object.getId() + "\"");
					//Path
					String path = StringUtils.nullToBlank(AppObjectUtil.getPath(object));
					out.println("  path=\"" + StringUtils.xmlEncode(path) + "\"");
					//Status Id
					if(object.getStatus() != null)					{
						out.println("  statusId=\"" + object.getStatus().getId() + "\"");
					}
					else {
						out.println("  statusId=\"0\"");
					}
					//Status
					if(object.getLockUser() != null) {	//改訂中
						out.println("  statusTypeName=\"" + EIMResource.getMessage(sess, "EIM.LABEL.REVISING", new Object[]{ object.getLockUser().getName() }) + "\"");
					}
					else if( expiration ) {		//有効期限切れ
						out.println("  statusTypeName=\"" + EIMResource.getMessage(sess, "EIM.LABEL.EXPIRATION_DATE_SLICE") + "\"");
					}
					else {	//通常のステータス
						if(object.getStatus() != null) {
							out.println("  statusTypeName=\"" + StringUtils.xmlEncode(object.getStatus().getType().getName()) + "\"");
						}
						else {
							out.println("  statusTypeName=\"-\"");
						}
					}
					//Security
					if(object.getSecurity() != null)
					{
						out.println("  securityId=\"" + object.getSecurity().getId() + "\"");
						out.println("  securityName=\"" + StringUtils.xmlEncode(object.getSecurity().getName()) + "\"");
					}
					//Lock User
					if(object.getLockUser() != null)
					{
						out.println("  lockUserName=\"" + StringUtils.xmlEncode(object.getLockUser().getName()) + "\"");
						out.println("  lockDate=\"" + object.getLockDate() + "\"");
					}
					out.println("  expiration=\"" + expiration + "\"");
					//Security Check For Read Only Role
					boolean readOnly = false;
					//アクセス権限チェック
					if (helper.isTypeOfFolder(object.getType())) {
						// ドキュメントのリレーションタイプ取得
						EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
						// 親リレーションの取得
						List parentRelList = RelationUtils.getParentRelationListByRelType(sess, object, relType,EIMAccessRole.READ);
						EIMObject parentObj = null;
						if (parentRelList != null && parentRelList.size() > 0) {
							// 親オブジェクトの取得
							parentObj = ((EIMRelation)parentRelList.get(0)).getParent();
						}
						if (parentObj != null) {
							// 下位フォルダ管理セキュリティ取得
							long sec_id = AppObjectUtil.getIntAttr(sess, parentObj, EIMConfig.get("ATTR_NAME_WORKSPACE_LOW_FOLDER_SEC"), Integer.MIN_VALUE);
							if (sec_id != Integer.MIN_VALUE) {
								EIMSecurity eimSec = SecurityUtils.getSecurityById(sess, sec_id);
								// Check Role
								if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
									readOnly = true;
								}
							}
						}
						//オブジェクトの書き込み権限をチェック
						if(object.getSecurity() != null)
						{
							if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
								readOnly = true;
							}
						}
					}
					else if (helper.isTypeOfWorkspace(object.getType())) {
						// 下位フォルダ管理セキュリティ管理
						if (!AppSecurityUtils.authorizedLowFolderSecurity(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
							readOnly = true;
						}
						
						//オブジェクトの書き込み権限をチェック
						if(object.getSecurity() != null) {
							if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
									readOnly = true;
							}
						}
						// セキュリティ管理
						if (readOnly)
						{
							boolean hasAuth = AdminAuthUtil.hasSpecifiedOrGeneralAuth(loginUser, AppConstant.ADMIN_AUTH_ID_SECURITY);
							if(!hasAuth) {
								readOnly = true;
							}
							else{
								readOnly = false;
							}
						}
					}
					else {
						if(object.getSecurity() != null) {
							if(!SecurityUtils.authorized(sess, object, sess.getUser(), EIMAccessRole.UPDATE)) {
								readOnly = true;
							}
						}
					}
					out.println("  readOnly=\"" + readOnly + "\"");

				out.println(" >");

					// 本I/Fでは出力しない属性のリストを作成する。
					List systemAttrList = new ArrayList();
					for(int ii = 0; ii < AppConstant.SYSTEM_ATTRIBUTE_DEFNAME.length; ii++) {
						systemAttrList.add(AppConstant.SYSTEM_ATTRIBUTE_DEFNAME[ii]);
					}
					for(int jj = 0; jj < readOnlySystemAttr.length; jj++) {
						systemAttrList.remove(EIMConfig.get(readOnlySystemAttr[jj]));
					}

					// オブジェクトの属性値を出力する
					//Root Node
					out.println("  <attList>");
					for(int i = 0; i < attTypeList.size(); i++)
					{
						EIMAttributeType attType = (EIMAttributeType)attTypeList.get(i);
						//属性がシステム属性である場合は出力しない
						boolean isSystemAttr = false;
						for(Iterator it = systemAttrList.iterator(); it.hasNext(); ) {
							String attNameTmp = (String)it.next();
							if( attType.getDefName().equals(attNameTmp) ) {
								isSystemAttr = true;
								break;
							}
						}
						if( isSystemAttr ){	//システム属性であればSkip
							continue;
						}

						EIMAttribute att = object.getAttribute(attType.getDefName());
						String value = "";
						StringBuffer attValueListStr = new StringBuffer();

						switch (attType.getValueType().getId()) {
						// 数値型
						case EIMValueType.INTEGER:
							if (att != null && att.getInts() != null && att.getInts().length > 0) {
								value = String.valueOf(att.getInts()[0]);

								long[] valueInts = TypeConvertUtils.convertToLongArray(att.getInts());
								for (int j = 0; j < valueInts.length; j++) {
									attValueListStr.append("     <attMultiple attValue=\"" + valueInts[j] + "\"/>");
								}
							}
							break;
							// double数値型
						case EIMValueType.DOUBLE:
							if (att != null && att.getDoubles() != null && att.getDoubles().length > 0) {
								value = String.valueOf(att.getDoubles()[0]);

								double[] valueDoubles = att.getDoubles();
								for (int j = 0; j < valueDoubles.length; j++) {
									attValueListStr.append("     <attMultiple attValue=\"" + FormatUtil.getDoubleFormatedString(valueDoubles[j]) + "\"/>");
								}
							}
							break;
						// 文字列型
						case EIMValueType.STRING:
							if (att != null && att.getStrings() != null && att.getStrings().length > 0) {
								value = att.getStrings()[0];

								String[] valueStrings = att.getStrings();
								for (int j = 0; j < valueStrings.length; j++) {
									attValueListStr.append("     <attMultiple attValue=\"" + StringUtils.xmlEncode(valueStrings[j]) + "\"/>");
								}
							}
							break;
						// 日付型
						case EIMValueType.DATE:
							if (att != null && att.getDates() != null && att.getDates().length > 0) {
								value = DateUtils.getDBTzToCLTzDate(sess, att.getDates()[0]);

								Date[] valueDates = att.getDates();
								for (int j = 0; j < valueDates.length; j++) {
									attValueListStr.append("     <attMultiple attValue=\""
											+ DateUtils.getDBTzToCLTzDate(sess, valueDates[j]) + "\"/>");
								}
							}
							break;
						// テキスト型
						case EIMValueType.TEXT:
							if (att != null && att.getTexts() != null && att.getTexts().length > 0) {
								value = att.getTexts()[0];

								String[] valueTexts = att.getTexts();
								for (int j = 0; j < valueTexts.length; j++) {
									attValueListStr.append("     <attMultiple attValue=\"" + StringUtils.xmlEncode(valueTexts[j]) + "\"/>");
								}
							}
							break;
						}
						if (attValueListStr.length() == 0) {
							attValueListStr.append(" <attMultiple attValue=\"\"/>");
						}
						//XML Out
						out.println("   <attribute");
							out.println("    attTypeId=\"" + attType.getId() + "\"");
							out.println("    attTypeName=\"" + StringUtils.xmlEncode(attType.getName()) + "\"");
							out.println("    attValue=\"" + StringUtils.xmlEncode(value) + "\"");
							out.println("    valTypeId=\"" + attType.getValueType().getId() + "\"");
							out.println("    valTypeName=\"" + StringUtils.xmlEncode(attType.getValueType().getName()) + "\"");
							out.println("    isMultiple=\"" + (attType.isMultiple()) + "\"");
						out.println("   >");
							out.println("    <attMultipleList>");
								out.println(attValueListStr.toString());
							out.println("    </attMultipleList>");
						out.println("   </attribute>");
					}

					//End Root Node
					out.println("  </attList>");

					//タグのリストを出力する
					//Root Node
					out.println("  <tagList>");
						long tagOids[] = AppObjectUtil.getIntAttrs(sess, object, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
						if(tagOids != null) {
							Arrays.sort(tagOids);	//ID順にソート
							for(int i = 0; i < tagOids.length; i++ ) {
								EIMObject tmpObj = ObjectUtils.getObjectById(sess, tagOids[i]);
								if(tmpObj == null || !SecurityUtils.authorized(sess, tmpObj, sess.getUser(), EIMAccessRole.READ)) {	//取得できなかった場合は権限がないものと判断する
									continue;
								}
								out.println("    <tagObject");
									out.println("     objId=\"" + tmpObj.getId() + "\"");
									out.println("     objName=\"" + StringUtils.xmlEncode(tmpObj.getName()) + "\"");
									out.println("     path=\"" + StringUtils.xmlEncode(AppObjectUtil.getPath(tmpObj)) + "\"");
								out.println("    />");
							}
						}
					out.println("  </tagList>");

				out.println(" </object>");
			out.println("</objList>");
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
