package servlet.external;

import java.io.File;
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

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.AppUpdateNoticeUtils;
import common.util.DisplayColorUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMFormat;
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
import eim.util.FileUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import eim.util.WorkFlowUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 外部インタフェース : ファイルアップロード
 *
 */
public class CreateDocumentEx extends HttpServlet
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
		String prmDocPath = EIMUtils.getParameter(request, "docPath");
		String prmProperty = EIMUtils.getParameter(request, "property");
		String prmExpireDate = EIMUtils.getParameter(request, "expireDate");
		//属性名・属性値もUTF-8に変換する
		String prmAttTypeName[] = request.getParameterValues("attTypeName");
		if(prmAttTypeName != null ) {
			for(int i = 0; i < prmAttTypeName.length; i++) {
				prmAttTypeName[i] = new String(prmAttTypeName[i].getBytes("iso-8859-1"), "UTF-8");
			}
		}
		String prmAttValue[] = request.getParameterValues("attValue");
		if(prmAttValue != null) {
			for(int j = 0; j < prmAttValue.length; j++) {
				prmAttValue[j] = new String(prmAttValue[j].getBytes("iso-8859-1"), "UTF-8");
			}
		}

		//パラメタ配列の作成
		ArrayList paramIdList = new ArrayList();
		paramIdList.add("userCode=" + prmUserCode);
		// パスワードがログに残るのは好ましくない筈
		// paramIdList.add("userPass=" + prmUserPass);
		paramIdList.add("folderPath=" + prmFolderPath);
		paramIdList.add("objTypeName=" + prmObjTypeName);
		paramIdList.add("docPath=" + prmDocPath);
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
			// パラメタチェック(必須パラメタの指定が無い、あるいはブランク値の場合はエラー)
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
			else if( StringUtils.nullToBlank(prmDocPath).equals("") ) {
				lackedParam = "docPath";
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

			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			//オブジェクトタイプの取得
			EIMObjectType objType = null;
			if ( prmObjTypeName != null ) {
//				//「一般ドキュメント」が指定された場合
				if( prmObjTypeName.equals(EIMResource.getMessage(sess, "EIM.OBJECTTYPE.GENERAL")) ) {
					objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
				}
				else {	//そうでない場合
					objType = ObjectUtils.getObjectTypeByName(sess, prmObjTypeName);
					// オブジェクトタイプがドキュメントのものかどうかチェック
					if(objType != null && !helper.isTypeOfDocument(objType)) {
						message = EIMResource.getMessage("EIM.ERROR.LOGIC.EXTIF.INVALIDTYPE");
						log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
						message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.EXTIF.INVALIDTYPE");
						out.println(AppMessageUtils.makeErrorTagByMessage(message));
						return;
					}
				}
			}
			if( objType == null ) {	//タイプが取得できなかった場合
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCUMENTTYPE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODOCUMENTTYPE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			//親オブジェクトの取得
			EIMObject parentObj = AppObjectUtil.getObjListByFullPass(sess, prmFolderPath, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
			if(parentObj == null) {
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOFOLDER");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOFOLDER");
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

			//ワークフロー付きフォルダ下の場合は、編集中以外は作成できない
			if (parentObj.getStatus() != null) {
				if (parentObj.getStatus().getType().getKind() != AppConstant.STATUS_TYPE_KIND_ID_EDITTING) {
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOCREATEROLE");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOCREATEROLE");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
				if (WorkFlowUtils.getWorkFlowByType(sess, objType) != null) {
					message = EIMResource.getMessage("EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
					log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
					message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.CANTSET.UNDERWF.WFDOCUMENT");
					out.println(AppMessageUtils.makeErrorTagByMessage(message));
					return;
				}
			}

			//フォーマット、ディレクトリの取得
			EIMFormat format = FileUtils.getDefaultFormat(sess, objType);
			if (format == null) {
				//デフォルトフォーマットが設定されていません。
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NODEFAULTFORMAT");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NODEFAULTFORMAT");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			//ファイル存在チェック
			File uploadFile = null;
			if( prmDocPath == null ||
					(uploadFile = new File(prmDocPath)) == null ||
					uploadFile.exists() == false)
			{
				message = EIMResource.getMessage("EIM.ERROR.LOGIC.NOUPLOADFILE");
				log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId));
				message = EIMResource.getMessage(sess, "EIM.ERROR.LOGIC.NOUPLOADFILE");
				out.println(AppMessageUtils.makeErrorTagByMessage(message));
				return;
			}

			//ファイル名、拡張子の取得
			String fileName = prmDocPath.substring(prmDocPath.lastIndexOf("/") + 1);
			String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(fileName));

			// Windows禁止文字チェック
			AppObjectUtil.checkValidateFName(sess, fileName);

			//指定されたドキュメントオブジェクトの作成
			EIMObject object = ObjectUtils.createObject(sess, objType, fileName);
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
			List existAttrList = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
			//その他の指定属性を設定
			if( prmAttTypeName != null && prmAttValue != null ) {
				for(int ii = 0; ii < Math.min(prmAttTypeName.length, prmAttValue.length); ii++) {
					attType = AttributeUtils.getAttributeTypeByName(sess, prmAttTypeName[ii]);
					if(attType != null) {
						// システム属性に該当するかどうかチェックする
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
						// オブジェクトタイプに存在する属性かどうかチェックする
						int kk = 0;
						for(; kk < existAttrList.size(); kk++) {
							if( attType.getDefName().equals(((EIMAttributeType)existAttrList.get(kk)).getDefName())) {
								break;
							}
						}
						if(kk >= existAttrList.size()) {
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

			//上位フォルダからの属性引継ぎ
			long[] parentLowAttrIds = AppObjectUtil.getIntAttrs(sess, parentObj, helper.getAttrNameOfToLowAttr());

			if (parentLowAttrIds != null) {
				//上位からの引継ぎ属性の設定内容に従い、parentObjから属性値をコピー
				//ただし、自身のタイプに該当属性が割り当てられているものに限る
				List parentLowAttrTypes = new ArrayList();
				{
					List parentLowAttrIdsInteger = new ArrayList(Arrays.asList(ArrayUtils.toObject(parentLowAttrIds)));
					List objectTypes = ObjectAttributeUtils.getAttributeTypeList(sess, objType);
					//引継ぎ対象を自身のタイプに該当属性が割り当てられているものにフィルタリング
					SearchToLowAttr:for (Iterator i = parentLowAttrIdsInteger.iterator(); i.hasNext();) {
						long attrTypeIdOfParentLowAttrId = ((Long)i.next()).longValue();
						for (Iterator j = objectTypes.iterator(); j.hasNext();) {
							EIMAttributeType attrTypeObj = (EIMAttributeType)j.next();
							if (attrTypeObj.getId() == attrTypeIdOfParentLowAttrId) {
								parentLowAttrTypes.add(attrTypeObj);
								continue SearchToLowAttr;
							}
						}
						i.remove();//自身のタイプに無かったので対象から削除
					}
					parentLowAttrIds = ArrayUtils.toPrimitive((Long[])parentLowAttrIdsInteger.toArray(new Long[parentLowAttrIdsInteger.size()]));
				}
				//「上位からの引継ぎ」属性の値を設定
				ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfFromHighAttr()
						, TypeConvertUtils.convertToBuildTypeArray(parentLowAttrIds));

				//各属性値の引継ぎ
				for (Iterator i = parentLowAttrTypes.iterator(); i.hasNext();) {
					EIMAttributeType attrType = (EIMAttributeType)i.next();
					EIMAttribute attr = parentObj.getAttribute(attrType.getDefName());
					if (attr != null) {		//上位に属性値がある場合は引継ぎ
						AppObjectUtil.setAttr(sess, object, attr);
					}
					else {	//上位が属性値を持たない場合は、属性を削除(attTypeName&attValueによる指定で設定されている場合を想定)
						AppObjectUtil.deleteAttribute(sess, object, attrType.getDefName());
					}
				}

				//リスト値表示色オブジェクトの引継ぎ
				DisplayColorUtil.inheritDisplayColor(sess, object, parentLowAttrTypes, parentObj);
			}

			//上位フォルダからのステータス引継ぎ
			if (parentObj.getStatus() != null) {
				WorkFlowUtils.updateObjectStatus(sess, object, parentObj.getStatus());

				//「上位WFフォルダ」属性も登録
				EIMAttribute attrOfHigherWFFolder = parentObj.getAttribute(helper.getAttrNameDocumentHigherWFFolder());
				if (attrOfHigherWFFolder == null) //WF付フォルダ直下
					ObjectAttributeUtils.setAttribute(sess, object, helper.getAttrTypeOfHigherWFFolder(), parentObj.getId());
				else //「WF付フォルダ下のフォルダ」の下
					AppObjectUtil.setAttr(sess, object, attrOfHigherWFFolder);
			}

			//セキュリティを設定
			EIMSecurity sec = parentObj.getSecurity();
			if(sec != null)
			{
				SecurityUtils.setSecurity(sess, object, sec);
			}

			//チェックイン実行（DBに登録）
			FileUtils.checkin(sess, object, format, fileName, uploadFile.length());//ここの第四引数はテーブルで論理的に保持しているのでどんな値でもよい

			//取得した実ファイルをファイルサーバ上に配置する（ファイルのコピー）
			//※ 将来的にSubVersion等から取得するような処理が必要になるかもしれない
			File srvFile = new File(format.getDirectory().getPath() + object.getId() + fileExt);
			FileUtils.copyFile(uploadFile, srvFile);

			// 作成したドキュメント自身にステータスが無く、かつ上位フォルダにもステータスが無い場合は、
			// WFなしドキュメントとして、即公開する
			if (object.getStatus() == null && parentObj.getStatus() == null) {

				// 公開ドキュメントとして登録
				EIMFormat publicDocumentFormat = FileUtils.getFormatByName(sess, EIMConfig.get("FORMAT_NAME_PUBLIC"));
				EIMFile file = FileUtils.getFile(sess, object, FileUtils.getDefaultFormat(sess, object.getType()));
				File orgFile = new File(file.getDirectory().getPath() + FileUtils.getFileName(object, file));
				File dstFile = new File(publicDocumentFormat.getDirectory().getPath() + object.getId() + file.getExt());
				FileUtils.copyFile(orgFile, dstFile);
				FileUtils.checkin(sess, object, publicDocumentFormat, file.getName(), file.getSize());
			}

			// SearchFramework 検索FW更新通知 対象：ドキュメント、登録先のフォルダorワークスペース
			AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_CREATE_DOCUMENT");
			AppUpdateNoticeUtils.updateNoticeInsertParent(sess, parentObj,
					"SEARCHFW_CREATE_DOCUMENT_PARENT_FOLDER", "SEARCHFW_CREATE_DOCUMENT_PARENT_WORKSPACE", null);

			//Access
			AccessUtils.createAccess(sess, object, "EIM.ACCESS.TYPE.INITIALREGIST");

			//Create Operation History
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.CREATE_DOCUMENT_EXCOMMAND,
					EIMConstant.TARGET_CREATE, EIMConstant.OBJECT, object,
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
