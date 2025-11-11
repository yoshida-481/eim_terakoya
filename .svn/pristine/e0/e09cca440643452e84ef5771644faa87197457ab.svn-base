package common.util;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import common.bo.TagTreeItem;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 
 * 署名・暗号化関連共通クラス
 * 
 */
public class SignUtil {
	
	// 更新通知用変数 タグの処理種別キー
	private static long rootObjId = 0;
	
	/**
	 * 引数タグ、および、その配下タグが「署名・暗号化済み」として良い状態の場合、「署名・暗号化済み」に更新します。
	 * 
	 * <li>既に署名・暗号化状態が「署名・暗号化済み」のタグの場合は更新しません。
	 * <li>当該タグの付与オブジェクトがフォルダ、または、「ドキュメント、かつ、署名・暗号化対象外」の場合、
	 *    「署名・暗号化済み」として良いと判断します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 「タグ」オブジェクト
	 * @throws Exception
	 */
	public static void setTagSignFlagOn(EIMSession sess, EIMObject obj) throws Exception {
		
		boolean sessPutFlg = false;
		
		if(EIMThreadContext.getEIMSession() == null){
			EIMThreadContext.putEIMSession(sess);
			sessPutFlg = true;
		}
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		HashSet objIdSet = new HashSet();	// 冗長なチェック回避用のHashSet [key]オブジェクトID
		
		// 配下タグを取得
		TagTreeItem item = TagUtil.getOnlyTagTree(sess, obj);

		// 再起呼び出し
		rootObjId = item.getEimObject().getId();
		setTagSignFlagOnRecurrently(sess, item, objIdSet, helper);
		
		
		if(sessPutFlg){
			EIMThreadContext.removeEIMSession();
		}
	}
	
	/**
	 * (再起処理)引数タグ、および、その配下タグが「署名・暗号化済み」として良い状態の場合、「署名・暗号化済み」に更新します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param item 配下のタグツリー構成要素
	 * @param objIdSet 冗長なチェック回避用のHashSet [key]オブジェクトID
	 * @param helper 条件判定ヘルパー
	 * @throws Exception
	 */
	private static void setTagSignFlagOnRecurrently(EIMSession sess, TagTreeItem item, 
			HashSet objIdSet, AppObjectConditionHelper helper) throws Exception {
		
		// 既にチェック済みのタグの場合は何もしない
		if (!objIdSet.contains(new Long(item.getEimObject().getId()))) {
		
			// ツリー末端まで辿り、末端から順番にチェック＆フラグ設定を実施
			for (Iterator iter = item.getTreeItemList().iterator(); iter.hasNext();) {
				TagTreeItem childItem = (TagTreeItem) iter.next();
				// 再起呼び出し
				setTagSignFlagOnRecurrently(sess, childItem, objIdSet, helper);
			}
			
			EIMObject tagObj = item.getEimObject();
			
			// 当該タグの「署名・暗号化状態」を取得
			long signencr = AppObjectUtil.getIntAttr(sess, tagObj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			
			// 既に署名・暗号化済みの場合は何もしない
			if (signencr != AppConstant.SIGNENCR_KIND_SIGNENCR) {
	
				boolean isSignFlagOn = true;
				
				// 当該タグの付与された全てのオブジェクトを取得
				List givenObjList = TagUtil.getTagGivenObj(sess, tagObj);
				
				// 付与オブジェクトがそれぞれ当該タグを署名・暗号化済みとして良い状態か否かをチェック
				for (Iterator iter = givenObjList.iterator(); iter.hasNext();) {
					EIMObject givenObj = (EIMObject) iter.next();
					if (!(isSignFlagOn = CheckSignEncrypt(sess, givenObj, helper))) {
						break;
					}
				}
				// 当該タグを「署名・暗号化済み」に更新する
				if (isSignFlagOn) {
					AppObjectUtil.setAttr(sess, tagObj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_SIGNENCR);
					AppUpdateNoticeUtils.updateNoticeInsert(tagObj.getId(), (String)EIMThreadContext.get("SEARCHFW.SIGNCHILDTAG.KEY"));
					if(tagObj.getId()==rootObjId)
					{
						// ルートタグのみ
						
						AppUpdateNoticeUtils.updateNoticeInsert(tagObj.getId(), (String)EIMThreadContext.get("SEARCHFW.SIGNTAG.KEY"));
					}
				}
			}
			// 冗長なチェック回避用セットに格納
			objIdSet.add(new Long(tagObj.getId()));
		}
	}
	
	/**
	 * 対象EIMObjectが、その親タグに対して「署名・暗号化済み」として良い状態か否かを判定します。
	 * 
	 * <li>対象がドキュメント、かつ、署名・暗号化対象外の場合はtrueを返却します。
	 * <li>対象がフォルダの場合にはtrueを返却します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 対象EIMObject
	 * @param helper 条件判定ヘルパー
	 * @return 親タグを「署名・暗号化済み」として良い状態か否か
	 * @throws Exception
	 */
	private static boolean CheckSignEncrypt (EIMSession sess, EIMObject obj, AppObjectConditionHelper helper) throws Exception {
		
		// 「ドキュメント、かつ、署名・暗号化対象外」、または、フォルダの場合、trueを返却
		if ((helper.isTypeOfDocument(obj.getType()) && !isSignEncrTarget(sess, obj)) || helper.isTypeOfFolder(obj.getType())) {
			return true;
		}
		
		// 属性「署名・暗号化状態」を取得
		long signencr = AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"),	 AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
		
		// 「署名・暗号化済み（処理成功）」の場合にのみtrueを返却
		if (signencr == AppConstant.SIGNENCR_KIND_SIGNENCR) {
			return true;
		} else {
			return false;
		}
	}
	
	/**
	 * 引数オブジェクトの全ての上位タグを「署名・暗号化していない」に更新します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 対象EIMObject
	 * @throws Exception
	 */
	public static void setParentTagSignFlagOff(EIMSession sess, EIMObject obj, String updateNoticeKey) throws Exception {
		
		// 全ての上位タグを取得
		List tagObjList = TagUtil.getParentTags(sess, obj);
		
		for (Iterator iter = tagObjList.iterator(); iter.hasNext();) {
			EIMObject tagObj = (EIMObject) iter.next();
			// 署名・暗号化状態を「署名・暗号化していない」に更新する
			AppObjectUtil.setAttr(sess, tagObj, EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS"), AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			AppUpdateNoticeUtils.updateNoticeInsert(tagObj.getId(), updateNoticeKey);
		}
	}
	
	/**
	 * 指定されたドキュメントが署名・暗号化対象かどうか判定します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param object 「ドキュメント」オブジェクト
	 * @return true(署名・暗号化対象)/false(署名・暗号化対象外)
	 * @throws Exception
	 */
	public static boolean isSignEncrTarget(EIMSession sess, EIMObject object) throws Exception {
		
		// 設定ファイルから署名・暗号化可能ファイル拡張子を取得
		String convert_file_type = EIMConfig.get("SIGNENCR_CONVERT_FILE_TYPE");
		
		if (convert_file_type != null) {
			String[] convFileTypeArray = convert_file_type.split(",");
			
			// 対象ドキュメントの拡張子を取得
			String fileExt = StringUtils.nullToBlank(StringUtils.getFileExt(object.getName()));
			
			// 拡張子が合致した場合はtrueを返却
			for (int i = 0 ; i < convFileTypeArray.length ; i++) {
				if(fileExt.equals("." + convFileTypeArray[i])) {
					return true;
				}
			}
		}
		return false;
	}
}