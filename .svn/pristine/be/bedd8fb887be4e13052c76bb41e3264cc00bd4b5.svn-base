package servlet.dl;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.bo.TagTreeItem;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.AppObjectConditionHelper;
import common.util.AppObjectUtil;
import common.util.CustomDefaultTableUtils;
import common.util.FormatUtil;
import common.util.TagUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMFile;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMTable;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMUtils;
import eim.util.FileUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SecurityUtils;
import eim.util.TableUtils;
import eim.util.UserUtils;
import eim.util.WorkFlowUtils;

/**
 * AllListをダウンロードするサーブレット
 * <p>
 * このクラスは
 * <code>{@link jakarta.servlet.http.HttpServlet}</code>
 * を拡張している。
 *
 * @version	1.0.0
 * @since		1.0.0
 */
public class DownloadExportList extends HttpServlet
{
	/**
	 * 宣言しないと怒られる
	 */
	private static final long serialVersionUID = 1L;

	// フォワード先JSP定義
	private final String ERROR_PAGE = "/servlet/HtmlErrorMessageServlet";

	// エラーメッセージ定義
	private final String ERROR_ATTR_KEY = "errorMessage";

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
	{
		// Error Logging
		Log log = LogFactory.getLog(this.getClass().getName());

		// Session
		EIMSession sess = null;
		EIMUser loginUser = null;

		// Parameter
		String prmObjId = request.getParameter("objId");

		//Message
		String message = null;
		Object[] paramId = {
				"objId=" + prmObjId
				};

		// Cache
		response.setHeader("Cache-Control", "max-age=0, must-revalidate");

		try {
			//Session
			sess = EIMUtils.getSession(request);
			if (sess == null) {
				message = EIMResource.getMessage(request, "EIM.ERROR.SESSIONTIMEOUT");
				log.warn(AppMessageUtils.makeLogMessage(message));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}
			//User
			loginUser = (EIMUser)sess.getAttribute("USER");

			//Object
			EIMObject object = ObjectUtils.getObjectById(sess, Long.parseLong(prmObjId));
			if(object == null)
			{
				message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.NOTAG");
				log.warn(AppMessageUtils.makeLogMessage(message));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			// アクセス権限
			if (!SecurityUtils.authorized(sess, object, loginUser, EIMAccessRole.READ))
			{
				message = EIMResource.getMessage(request, "EIM.ERROR.LOGIC.NOACCESS");
				log.warn(AppMessageUtils.makeLogMessage(message));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}

			/*AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
			//アクセス権チェック
			try {
				helper.checkAccessibleStatusSelf(object, true);
			} catch (EIMException e) {
				if (!e.getMessageKey().equals("EIM.ERROR.LOGIC.NOACCESS")) throw e;
				message = e.getMessage();
				log.warn(AppMessageUtils.makeLogMessage(message));
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
				return;
			}*/

			AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);

			//指定タグが付与されたオブジェクトのリストを取得する
			TagTreeItem items = TagUtil.getTagTreeWithChild(sess, object);
			//トップ-ファイル一覧と同じ順序になるようにソート
			TagUtil.sortTagTree(sess, items, false, helper);

			//ユーザーが現在選択しているテーブルを取得する
			EIMTable table = TableUtils.getSelectedTableByUser(sess, loginUser);
			List attList = null;
			//getSelectedTableByUser()で取得したテーブルには属性一覧が含まれないので
			//再度テーブルを取得する必要がある
			if( table != null && (table = TableUtils.getTableById(sess, table.getId())) != null) {
				attList = table.getAttributeList();
			}
			else {
				// ユーザーが現在選択しているカスタマイズデフォルトテーブルの定義名称を取得する
				String selectedCustomDefaultTableDefName = CustomDefaultTableUtils.getSelectedTableDefNameByUser(sess, loginUser);

				if(selectedCustomDefaultTableDefName != null){
					// 選択しているカスタマイズデフォルトテーブルの属性タイプリストを取得
					attList = CustomDefaultTableUtils.getSelectedTableAttributeTypeListByUser(sess, loginUser);
				}else{
					// テーブルが取得できない場合(デフォルトテーブルの場合)

					attList = new ArrayList();
					// プロパティ
					attList.add(AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PROP")));
					// 更新者
					attList.add(AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER")));
					// 更新日
					attList.add(AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE")));
				}

			}

			//出力用XMLの作成
			List xmlList = new ArrayList();
			if( EIMConfig.get("ALLLIST_XMLDEC_ENCODING_PROP") != null )
				//XML宣言の出力
				xmlList.add("<?xml version=\"1.0\" encoding=\"" + EIMConfig.get("ALLLIST_XMLDEC_ENCODING_PROP") + "\"?>");
			xmlList.addAll(createXMLListRecursive(sess, items, attList, helper, 0));

			Date date = new Date();
			SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMddHHmmss");
			// ファイル名のエンコード
			String fileName = items.getEimObject().getName() + "_" + sdf.format(date) + ".xml";
			String fileNameEncode = "";
			if (EIMConfig.get("DOWNLOAD_FILENAME_ENCODE_JAP_ONLY")
					.equals(AppConstant.DOWNLOAD_FILENAME_ENCODE_JAP_ONLY_ON)) {
				// MS932でエンコード
				// (英語Windowsで日本語ファイル名をダウンロードすると文字化け発生)
				fileNameEncode = new String(fileName.getBytes(AppConstant.ENCODE_MS932), "ISO-8859-1");
			} else {
				// UTF-8でエンコード
				// (日本語ファイル名のファイルを「開く」とファイル名は文字化けしている。保存するときは問題なし。)
				fileNameEncode = URLEncoder.encode(fileName, AppConstant.ENCODE_UTF8).replace("+", "%20");
			}

			response.setContentType("application/octet-stream");
			response.setHeader("Content-Disposition", "attachment; filename=\"" + fileNameEncode + "\"; filename*="
					+ AppConstant.ENCODE_UTF8 + "\'\'"
					+ URLEncoder.encode(fileName, AppConstant.ENCODE_UTF8).replace("+", "%20"));

			//OutputStream
			OutputStream out = response.getOutputStream();
			BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(out, EIMConfig.get("ALLLIST_ENCODING")));
			for(Iterator i = xmlList.iterator(); i.hasNext(); ) {
				String outString = (String)i.next();
				bw.write(outString);	// 書き込み
				bw.newLine();			// 改行
				bw.flush();
			}
			bw.close();
			out.close();

			//操作履歴
			OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.EXPORT_LIST,
					AppConstant.TARGET_TO_EXPORT, EIMConstant.OBJECT, object,
					null, null, null, AppObjectUtil.getPath(object));

			//Commit
			sess.commit();
		}
		catch(EIMException eime)
		{
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
			}

			message = eime.getMessage();
			log.warn(AppMessageUtils.makeLogMessage(loginUser.getId(), message, paramId), eime);
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
		}
		catch(Exception e)
		{
			try{
				if(sess != null){
					sess.rollback();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
			}
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
			request.setAttribute(ERROR_ATTR_KEY, message);
			getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
		}
		finally
		{
			try{
				if(sess != null){
					sess.close();
				}
			}
			catch (Exception se) {
				log.error(AppMessageUtils.makeLogMessage(se.getMessage()), se);
				message = EIMResource.getMessage("EIM.ERROR.SYSTEMERROR");
				request.setAttribute(ERROR_ATTR_KEY, message);
				getServletContext().getRequestDispatcher(ERROR_PAGE).forward(request, response);
			}
		}
	}

	protected void doGet(	HttpServletRequest	request,
												HttpServletResponse	response)
	throws ServletException, IOException
	{
		doPost(request, response);
	}

	/**
	 * 引数で指定されたTagTreeItemから再帰的にXMLを作成する
	 * ※ 出力する1行(≒1要素)ごとにStringオブジェクトを作成し、List化する
	 * ※ DOMを使用しても良いが、属性の出力順序を制御できないという問題がある
	 *
	 * @param sess EIMSessionインスタンス
	 * @param item 対象のTagTreeItemオブジェクト
	 * @param attList 出力する属性のリスト
	 * @param helper AppObjectConditionHelperクラスのインスタンス
	 * @param level 何階層目の処理かを示す数値(トップは0)
	 * @throws Exception
	 */
	private List createXMLListRecursive(EIMSession sess, TagTreeItem item, List attList, AppObjectConditionHelper helper, int level) throws Exception
	{
		List retList = new ArrayList();	//返却用リスト

		String outString = "";	//出力行
		String strIndent = "";	//インデント
		String attName = "";	//属性名称
		EIMObject obj = item.getEimObject();

		//インデントをつける
		for(int i = 0; i < level; i++) {
			strIndent += "\t";
		}
		outString = strIndent;

		//オブジェクトタイプの決定
		String type = "document";
		if( helper.isTypeOfFolder(obj.getType()) ) {
			type = "folder";
		}
		else if( helper.isTypeOfTag(obj.getType()) ) {
			type = "tag";
		}
		outString += "<" + type + " ";

		// 属性値の設定(オブジェクト名・履歴・ステータスの3つは固定)
		//オブジェクト名
		attName = EIMResource.getMessage(EIMConfig.get("ALLLIST_LANG"), "EIM.LABEL.ALLLIST.ATTNAME.NAME");
		outString += "attTypeName1=\"" + attName + "\" attValue1=\"" + obj.getName() + "\" ";
		//履歴
		attName = EIMResource.getMessage(EIMConfig.get("ALLLIST_LANG"), "EIM.LABEL.ALLLIST.ATTNAME.REV");
		if(type.equals("document"))
			outString += "attTypeName2=\"" + attName + "\" attValue2=\"" + obj.getRev() + "\" ";
		else
			outString += "attTypeName2=\"" + attName + "\" attValue2=\"-\" ";
		//ステータス
		attName = EIMResource.getMessage(EIMConfig.get("ALLLIST_LANG"), "EIM.LABEL.ALLLIST.ATTNAME.STATUS");
		if( obj.getStatus() != null && obj.getStatus().getType() != null ) {
			String stsName = WorkFlowUtils.getOtherStatusTypeName(sess, obj.getStatus().getType().getId(), EIMConfig.get("ALLLIST_LANG"));
			outString += "attTypeName3=\"" + attName + "\" attValue3=\"" + stsName + "\" ";
		}
		else {
			outString += "attTypeName3=\"" + attName + "\" attValue3=\"-\" ";
		}

		// その他の属性値の設定(4オリジン)
		if(attList != null) {
			int num = 4;
			for (Iterator i = attList.iterator(); i.hasNext();) {
				EIMAttributeType attType = ((EIMAttributeType) i.next());
				//設定言語による属性名の取得
				attName = AttributeUtils.getOtherAttTypeName(sess, attType.getId(), EIMConfig.get("ALLLIST_LANG"));
				if(attName == null) {
					attName = attType.getDefaultName();
				}

				//「作成者」「作成日」「更新者」「更新日」「サイズ」の処理  ※dspChildObject.jsp あたりと処理の共通化ができないものか？
				if( attType.getDefName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE")) ) {	//作成者
					long uId = 0;
					if(type.equals("folder")) {	//フォルダの場合
						uId = obj.getCreateUser().getId();
					}
					else {	//ドキュメント・タグの場合
						uId = obj.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE")).getInt();
					}
					String uName = UserUtils.getOtherUserName(sess, uId, EIMConfig.get("ALLLIST_LANG"));
					outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + uName + "\" ";
					num++;
					continue;
				}
				else if( attType.getDefName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE_DATE")) ) {	//作成日
					Date cDate = obj.getCreateDate();
					//DB時間をAPPサーバー上の時間に変換する
					long aTime = DateUtils.convDBTzToGmtTime(sess, cDate.getTime()) + DateUtils.getAPTzOffset();
					String fTime = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.format(new Date(aTime));	//ISO-8601 形式で出力
					outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + fTime + "\" ";
					num++;
					continue;
				}
				else if( attType.getDefName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_USER")) ) {	//更新者
					long uId = obj.getModifyUser().getId();
					String uName = UserUtils.getOtherUserName(sess, uId, EIMConfig.get("ALLLIST_LANG"));
					outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + uName + "\" ";
					num++;
					continue;
				}
				else if( attType.getDefName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_MODIFY_DATE")) ) {	//更新日
					Date mDate = obj.getModifyDate();
					long aTime = DateUtils.convDBTzToGmtTime(sess, mDate.getTime()) + DateUtils.getAPTzOffset();
					String fTime = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.format(new Date(aTime));	//ISO-8601 形式で出力
					outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + fTime + "\" ";
					num++;
					continue;
				}
				else if( attType.getDefName().equals(EIMConfig.get("ATTR_NAME_DOCUMENT_SIZE")) ) {	//サイズ
					long fSize = 0;
					if(type.equals("document")) {	//ドキュメントの場合
						EIMFile file = FileUtils.getFile(sess, obj, FileUtils.getDefaultFormat(sess,obj.getType()));
						if( file != null )	//結合失敗ドキュメントの場合はEIMFileを取得できないことによる処置
							fSize = file.getSize();
					}
					outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + fSize + "\" ";
					num++;
					continue;
				}

				//属性値の取得
				switch (attType.getValueType().getId()) {
					// 文字列・テキスト型の場合
					case EIMValueType.STRING:
					case EIMValueType.TEXT:
						String sVal = "";
						if( obj.getAttribute(attType.getDefaultName()) != null ) {
							if(attType.isMultiple())	// 複数値属性の場合はトップの値のみ取得
								sVal = obj.getAttribute(attType.getDefaultName()).getStrings()[0];
							else
								sVal = obj.getAttribute(attType.getDefaultName()).getString();
						}
						outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + sVal + "\" ";
						num++;
						break;
					// 数値型の場合
					case EIMValueType.INTEGER:{
						long nVal = -1;
						if( obj.getAttribute(attType.getDefaultName()) != null ) {
							if(attType.isMultiple())	// 複数値属性の場合はトップの値のみ取得
								nVal = obj.getAttribute(attType.getDefaultName()).getInts()[0];
							else
								nVal = obj.getAttribute(attType.getDefaultName()).getInt();
							outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + nVal + "\" ";
						}
						else {
							outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"\" ";
						}
						num++;}
						break;
						// double数値型の場合
					case EIMValueType.DOUBLE:{
						double nVal = 0;
						if( obj.getAttribute(attType.getDefaultName()) != null ) {
							if(attType.isMultiple())	// 複数値属性の場合はトップの値のみ取得
								nVal = obj.getAttribute(attType.getDefaultName()).getDoubles()[0];
							else
								nVal = obj.getAttribute(attType.getDefaultName()).getDouble();
							outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + FormatUtil.getDoubleFormatedString(nVal) + "\" ";
						}
						else {
							outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"\" ";
						}
						num++;}
						break;
						// 日付型の場合
					case EIMValueType.DATE:
						Date dVal = new Date();
						if( obj.getAttribute(attType.getDefaultName()) != null ) {
							if(attType.isMultiple())	// 複数値属性の場合はトップの値のみ取得
								dVal = obj.getAttribute(attType.getDefaultName()).getDates()[0];
							else
								dVal = obj.getAttribute(attType.getDefaultName()).getDate();

							long aTime = DateUtils.convDBTzToGmtTime(sess, dVal.getTime()) + DateUtils.getAPTzOffset();
							String fTime = DateFormatUtils.ISO_DATETIME_TIME_ZONE_FORMAT.format(new Date(aTime));	//ISO-8601 形式で出力
							outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"" + fTime + "\" ";
						}
						else {
							outString += "attTypeName" + num + "=\"" + attName + "\" attValue" + num + "=\"\" ";
						}
						num++;
						break;
				}
			}
		}

		List childItemList = null;
		if( (childItemList = item.getTreeItemList()) != null && childItemList.size() > 0) {

			outString += ">";
			retList.add(outString);
			//再帰処理
			for (Iterator i = childItemList.iterator(); i.hasNext();) {
				TagTreeItem childItem = (TagTreeItem)(i.next());
				retList.addAll( createXMLListRecursive(sess, childItem, attList, helper, level+1 ) );
			}
			outString = strIndent;
			outString += "</" + type + ">";
			retList.add(outString);
		}
		else {
			outString += "/>";
			retList.add(outString);
		}

		return retList;
	}
}