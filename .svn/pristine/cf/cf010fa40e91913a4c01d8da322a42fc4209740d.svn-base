package servlet.external;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
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
import common.util.SignUtil;
import common.util.TagUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 外部インタフェース : タグ付与
 *
 */
public class AssignTagEx extends HttpServlet
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
		String prmTargetType[] = request.getParameterValues("targetType");
		if(prmTargetType != null ) {
			for(int ii = 0; ii < prmTargetType.length; ii++) {
				prmTargetType[ii] = new String(prmTargetType[ii].getBytes("iso-8859-1"), "UTF-8");
			}
		}
		String prmTargetPath[] = request.getParameterValues("targetPath");
		if(prmTargetPath != null) {
			for(int ii = 0; ii < prmTargetPath.length; ii++) {
				prmTargetPath[ii] = new String(prmTargetPath[ii].getBytes("iso-8859-1"), "UTF-8");
			}
		}
		String prmTagPath[] = request.getParameterValues("tagPath");
		if(prmTagPath != null) {
			for(int ii = 0; ii < prmTagPath.length; ii++) {
				prmTagPath[ii] = new String(prmTagPath[ii].getBytes("iso-8859-1"), "UTF-8");
			}
		}

		//パラメタ配列の作成
		ArrayList paramIdList = new ArrayList();
		paramIdList.add("userCode=" + prmUserCode);
		// パスワードがログに残るのは好ましくない筈
		// paramIdList.add("userPass=" + prmUserPass);
		if(prmTargetType != null && prmTargetPath != null ) {
			for(int k = 0 ; k < Math.min(prmTargetType.length, prmTargetPath.length) ; k++)
			{
				paramIdList.add("targetType[" + k + "]=" + prmTargetType[k]);
				paramIdList.add("targetPath[" + k + "]=" + prmTargetPath[k]);
			}
		}
		if(prmTagPath != null) {
			for(int k = 0 ; k < prmTagPath.length; k++)
			{
				paramIdList.add("tagPath[" + k + "]=" + prmTagPath[k]);
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
			else if( prmTargetType == null || hasBlankValue(prmTargetType) ) {
				lackedParam = "targetType";
			}
			else if( prmTargetPath == null || hasBlankValue(prmTargetPath) ) {
				lackedParam = "targetPath";
			}
			else if( prmTagPath == null || hasBlankValue(prmTagPath) ) {
				lackedParam = "tagPath";
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

			StringBuffer sb = new StringBuffer();
			sb.append("<assignList>");

			// 条件判定ヘルパー作成
			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			//付与するタグの数分ループ
			HashMap typeMap = new HashMap();
			typeMap.put("document", EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
			typeMap.put("folder", EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
			typeMap.put("tag", EIMConfig.get("OBJECT_TYPE_NAME_TAG"));

			//指定されたタイプとパスの数が違う場合、
			//少ないほうに合わせる
			//  2009/04/17 modified by fujii
			//  2009/07/27 moved by mukai  変数チェック前に実行していたので、位置をずらした
			int min = Math.min(prmTargetType.length, prmTargetPath.length);
			String[] tmpType = new String[min];
			String[] tmpPath = new String[min];
			System.arraycopy(prmTargetType, 0, tmpType, 0, min);
			System.arraycopy(prmTargetPath, 0, tmpPath, 0, min);
			prmTargetType = tmpType;
			prmTargetPath = tmpPath;
			
			// 付与元のタグ取得のためにタイプ配列を作成
			String[] tagTypes = new String[prmTagPath.length];
			for(int ii = 0; ii < prmTagPath.length; ii++)
			{
				tagTypes[ii] = EIMConfig.get("OBJECT_TYPE_NAME_TAG");
			}

			// 付与元のタグを取得
			List tagObjList = AppObjectUtil.getObjListByFullPass(sess, prmTagPath, tagTypes);

			if (tagObjList == null || tagObjList.size() == 0) {
				// 選択したタグを取得できませんでした。
				throw new Exception(EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAG"));
			}

			// 取得漏れの有無をチェック
			HashSet tagPathSet = objListToPathHashSet(tagObjList);
			for (int ii = 0 ; ii < prmTagPath.length ; ii++) {
				if (!tagPathSet.contains(deleteLastSlash(prmTagPath[ii]))) {
					// 選択したタグ({0})を取得できませんでした。
					String[] msg = {prmTagPath[ii]};
					throw new EIMException(sess, "EIM.ERROR.LOGIC.NOSELECTEDTAG", msg);
				}
			}

			// 属性タイプの冗長な取得回避用Mapを生成
			HashMap attrTypeMap = TagUtil.getTagAttrTypeMap(sess);

			boolean isTagSignFlagOff = false;

			// 付与元タグの個数分ループ
			for (Iterator iter = tagObjList.iterator(); iter.hasNext();) {
				EIMObject tagObj = (EIMObject) iter.next();

				// 権限チェック
				if (tagObj.getSecurity() != null) {
					if (!SecurityUtils.authorized(sess, tagObj, sess.getUser(),EIMAccessRole.UPDATE)) {
						throw new Exception(EIMResource.getMessage("EIM.ERROR.LOGIC.NOTAGASSIGNROLE"));
					}
				}

				sb.append("<assign");
				sb.append(" tagObjId=\"" + tagObj.getId() + "\"");
				sb.append(">");

				sb.append("<targetList>");

				// 付与対象オブジェクトの取得のためにタイプ配列を作成
				String[] targetTypes = new String[prmTargetPath.length];
				for (int ii = 0; ii < prmTargetPath.length; ii++) {
					targetTypes[ii] = (String)typeMap.get(prmTargetType[ii]);
					if(targetTypes[ii] == null || targetTypes[ii].length() == 0) {
						// 選択したオブジェクト({0})は存在しません。
						String[] msg = {prmTargetType[ii]};
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOSELECTEDOBJECT", msg);
					}
				}

				// 付与対象オブジェクトの取得
				List targetObjList = AppObjectUtil.getObjListByFullPass(sess, prmTargetPath, targetTypes);

				if (targetObjList == null || targetObjList.size() == 0) {
					// 選択したドキュメント、フォルダまたはタグは存在しません。
					throw new Exception(EIMResource.getMessage("EIM.ERROR.LOGIC.NODOCORFOLORTAG"));
				}

				// 取得漏れの有無をチェック
				HashSet targetPathSet = objListToPathHashSet(targetObjList);
				for (int ii = 0 ; ii < prmTargetPath.length ; ii++) {
					if (!targetPathSet.contains(deleteLastSlash(prmTargetPath[ii]))) {
						// 選択したオブジェクト({0})は存在しません。
						String[] msg = {prmTargetPath[ii]};
						throw new EIMException(sess, "EIM.ERROR.LOGIC.NOSELECTEDOBJECT", msg);
					}
				}

				// タグの更新通知のための設定
				EIMThreadContext.put("SEARCHFW.TAG.PATTERN", "SelectTagEx");
				// 付与対象オブジェクトの個数分ループ
				for (Iterator iterator = targetObjList.iterator(); iterator.hasNext();) {
					EIMObject targetObj = (EIMObject) iterator.next();

					//オブジェクトがワークスペースだった場合はタグ付与ができない
					if( helper.isTypeOfWorkspace(targetObj.getType()) )
						throw new EIMException(sess, "EIM.ERROR.LOGIC.EXTIF.CANT.ASSIGN.TAG.TO.WORKSPACE", new Object[]{ targetObj.getName() });

					//タグを付与する（フォルダの場合は再帰的に処理）
					isTagSignFlagOff = isTagSignFlagOff | TagUtil.assignTag(sess, targetObj, tagObj, loginUser, attrTypeMap, helper);

					// アクセス履歴はassignTag()の内部で登録されるため、本関数での処理は不要

					sb.append("<targetObj objId=\"" + targetObj.getId() + "\" />");
				}

				sb.append("</targetList>");

				TagUtil.getOnlyTagTree(sess,tagObj);

				// duplicate name check
				TagUtil.isDupObjNameUnderTag(sess, tagObj);

				sb.append("</assign>");

				if (isTagSignFlagOff) {

					long signencr = AppObjectUtil.getIntAttr(sess, tagObj, helper.getAttrNameOfSignEncStatus(),
							AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
					if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR) {
						// 署名・暗号化済の場合、署名・暗号化未実施に戻す
						AppObjectUtil.setAttr(sess, tagObj, helper.getAttrNameOfSignEncStatus(), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);

						// 親タグの署名・暗号化状態を更新
						SignUtil.setParentTagSignFlagOff(sess, tagObj ,"SEARCHFW_SELECTTAG_PARENT_TAG");

					}
				}

				// SearchFramework 検索FW更新通知 対象タグ
				AppUpdateNoticeUtils.updateNoticeInsert(tagObj.getId(), "SEARCHFW_SELECTTAG_TAG");

				// 操作履歴(付与元タグ毎に出力する)
				String path = AppObjectUtil.getPath(tagObj);
				OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.ADD_TAG_EXCOMMAND,
						AppConstant.ADDED_TAG, EIMConstant.OBJECT, tagObj,
						null, null, null,
						path);
			}

			sb.append("</assignList>");

			//処理成功時のXMLを出力
			out.println(sb.toString());

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

	/**
	 * EIMObjectのリストから「<パス属性> + <オブジェクト名>」のHashSetを作成します。
	 *
	 * <li>HashSetには「<パス属性> + <オブジェクト名>」を格納します。
	 *
	 * @param objList EIMObjectのリスト
	 * @return 各EIMObjectの「<パス属性> + <オブジェクト名>」を格納したHashSet
	 */
	private static HashSet objListToPathHashSet(List objList) {

		HashSet set = new HashSet();
		if (objList != null) {
			for (Iterator iter = objList.iterator(); iter.hasNext();) {
				EIMObject obj = (EIMObject) iter.next();
				String tmpPath = AppObjectUtil.getPath(obj);
				if( tmpPath == null ) {
					tmpPath = "/";	//ワークスペースの場合
				}
				set.add(tmpPath + obj.getName());
			}
		}
		return set;
	}

	/**
	 * 引数文字列から最後尾の'/'を除去します。
	 *
	 * @param str 最後尾に'/'が付くと想定される文字列
	 * @return 最後尾の'/'を除去した文字列
	 */
	private static String deleteLastSlash(String str) {

		if (!StringUtils.isBlank(str)) {
			if (str.charAt(str.length() - 1) == '/') {
				str = str.substring(0, str.length() - 1);
			}
		}
		return str;
	}

	/**
	 * 複数のユーザー指定値の中にブランク値があるかどうかを返します
	 *
	 * @param strArray 指定値配列
	 * @return ブランク値があればtrue
	 */
	private static boolean hasBlankValue(String[] strArray) {

		if( strArray == null )
			return true;

		for(int i = 0; i < strArray.length; i++) {
			if( strArray[i].equals("") ) {
				return true;
			}
		}
		return false;
	}

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		doPost(request, response);
	}
}