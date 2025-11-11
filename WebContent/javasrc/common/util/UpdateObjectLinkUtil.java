package common.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import jakarta.servlet.http.HttpServletRequest;

import app.document.object.UpdateObjectLink;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMRelation;
import eim.bo.EIMRelationType;
import eim.bo.EIMVersion;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.OperationHistoryUtils;
import eim.util.RelationUtils;
import eim.util.TypeConvertUtils;
import eim.util.VersionUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;



/**
 * オブジェクトリンク最新化ユーティリティクラス
 * @author fujimoto
 *
 */
public class UpdateObjectLinkUtil {

	/**
	 * リクエストパラメータからオブジェクトリンクのリストを取得する
	 * @param sess
	 * @param request
	 * @return
	 */
	static public List getUpdateObjectLinkList(EIMSession sess, HttpServletRequest request)
	throws Exception
	{
		ArrayList linkList = new ArrayList();
		int numObject = 0;
		long commonParentObjId = 0;
		boolean isCommonParent;	// 共通の親オブジェクトを使用するか

		// 選択したオブジェクトリンクの数
		String numObjectStr = EIMUtils.getParameter(request, "objNum");
		numObject = Integer.parseInt(numObjectStr);

		// 共通親オブジェクトID
		String parentObjIdStr = EIMUtils.getParameter(request, "parentObjId");
		isCommonParent = (parentObjIdStr != null && (parentObjIdStr.compareTo("") != 0)) ? true : false;
		if (isCommonParent) {
			commonParentObjId = Long.parseLong(parentObjIdStr);
		}

		for (int i = 0; i < numObject; i++)
		{
			long objId;
			long parentObjId;

			// リンク元オブジェクトID
			String objIdStr = EIMUtils.getParameter(request, "objId_" + i);
			objId = Long.parseLong(objIdStr);

			// 個別に親オブジェクトを使用するケース(改訂履歴画面)
			if (isCommonParent == false)
			{
				parentObjIdStr = EIMUtils.getParameter(request, "parentObjId_" + i);
				parentObjId = Long.parseLong(parentObjIdStr);
			}
			// 共通の親オブジェクトを使用するケース(トップ画面)
			else
			{
				parentObjId = commonParentObjId;
			}

			// ドキュメント最新化クラスを作成し、リストに登録
			UpdateObjectLink updateObjLink = new UpdateObjectLink(sess, objId, parentObjId);
			linkList.add(updateObjLink);
		}

		return linkList;
	}

	/**
	 * オブジェクトリンク最適化を行う
	 * @param updateObjLinkList
	 * @return 最新化後のオブジェクトIDリスト
	 * @throws Exception
	 * @throws EIMException
	 */
	static public List actUpdateObjectLinkList(EIMSession sess, List updateObjLinkList)
	throws Exception, EIMException
	{
		boolean sessPutFlg = false;
		
		if(EIMThreadContext.getEIMSession() == null){
			EIMThreadContext.putEIMSession(sess);
			sessPutFlg = true;
		}
		
		// 最新履歴オブジェクトの重複チェック
		boolean isDuplicateLatest;
		isDuplicateLatest = isDuplicationLatestObject(updateObjLinkList);
		if (isDuplicateLatest)
		{
			// 最新履歴が同じで、かつパスが重複しているオブジェクトが存在する
			throw new EIMException(sess, "EIM.ERROR.LOGIC.DUPLICATELATESTOBJPATH");
		}

		/* 更新オブジェクト重複チェック用HashMap */
		HashMap duplicateChkMap = new HashMap();

		/* 更新後オブジェクトID保持用List */
		List latestObjIdList = new ArrayList<Long>();

		/* 選択したオブジェクト個々に対して行う */
		for (int i = 0; i < updateObjLinkList.size(); i++)
		{
			// 各種チェックの結果
			boolean isRoleLegal;
			boolean isExistLink;
			boolean latestIsSamePath;

			// オブジェクトリンク最新化クラスの取得
			UpdateObjectLink updateObjLink;
			updateObjLink = (UpdateObjectLink)updateObjLinkList.get(i);

			// 更新対象重複チェック(最新履歴のオブジェクトは常に更新されるのでチェック無し)
			String objIdStr = Long.toString(updateObjLink.getObjectId());
			updateObjLink.reloadLatestObj();
			if (duplicateChkMap.containsKey(objIdStr)) {
				updateObjLink.reloadObject();
			}
			else {
				duplicateChkMap.put(objIdStr, objIdStr);
			}

			// 権限チェック
			// チェックの過程でリンク元、リンク先を取得するので
			// リンク元、リンク先の存在チェックはこの中で行う
			isRoleLegal = updateObjLink.isRoleLegal();
			if (isRoleLegal == false)
			{
				// 権限が不正
				throw new EIMException(updateObjLink.getSession(), "EIM.ERROR.LOGIC.NOUPDATEDOCLINKROLE");
			}

			// オブジェクトリンクが存在するかのチェック
			isExistLink = updateObjLink.isExistDocumentLink();
			if (isExistLink == false)
			{
				// ドキュメントリンクが存在しない
				Object args[] = {updateObjLink.getObject().getName()};
				throw new EIMException(updateObjLink.getSession(), "EIM.ERROR.INPUT.NODOCLINK", args);
			}

			// 最新の履歴のドキュメントリンクであるなら何もしない
			if (updateObjLink.getLatestObj().getId() != updateObjLink.getObjectId())
			{
				// 同じディレクトリ内に最新版のドキュメントリンクが存在するかどうか確認する
				latestIsSamePath = updateObjLink.latestObjIsSamePath();
				if (latestIsSamePath == true)
				{
					// 最新版のドキュメントリンクが同じディレクトリ内にある
					Object args[] = {updateObjLink.getObject().getName()};
					throw new EIMException(updateObjLink.getSession(), "EIM.ERROR.LOGIC.LATESTOBJISSAMEPATH", args);
				}

				// ドキュメントリンクを最新化
				// アクセス履歴もこの中で登録する
				
				updateObjLink.updateLink();

				// 操作履歴の登録
				OperationHistoryUtils.create(sess, AppConstant.DOCUMENT, AppConstant.DOCLINK_UPDATE,
						AppConstant.LINK_SOURCE, EIMConstant.OBJECT, updateObjLink.getObject(),
						AppConstant.LINK_ORIGIN, EIMConstant.OBJECT, updateObjLink.getParenObject(),
						AppObjectUtil.getPath(updateObjLink.getLatestObj()));
			}
			latestObjIdList.add(updateObjLink.getLatestObj().getId());
		}
		
		if(sessPutFlg){
			EIMThreadContext.removeEIMSession();
			sessPutFlg = false;
		}

		return latestObjIdList;
	}

	/**
	 * 最新履歴のオブジェクトの重複が無いかチェックします
	 * @param updateObjList
	 * @return 重複していればtrueを返します
	 * @throws Exception
	 * @throws EIMException
	 */
	static private boolean isDuplicationLatestObject(List updateObjList)
	throws Exception, EIMException
	{
		// 最新履歴のオブジェクト情報を格納するハッシュマップ
		// チェイン法で格納する
		HashMap latestObjMap = new HashMap();

		// 全てのオブジェクトに対し、最新履歴のオブジェクトを取得し
		// 重複が無いかチェックする
		for (int i = 0; i < updateObjList.size(); i++)
		{
			List parentPathList;
			UpdateObjectLink updateObjLink = (UpdateObjectLink)updateObjList.get(i);

			// 最新履歴オブジェクトと、リンク元のパスを取得
			String latestObjIdStr = Long.toString(updateObjLink.getLatestObj().getId());
			String crntParentPath = updateObjLink.getParentPath();

			if (latestObjMap.containsKey(latestObjIdStr))
			{
				// 登録しているリストを取り出し、パスが等しいものがあるかチェック
				parentPathList = (List)latestObjMap.get(latestObjIdStr);

				for (int j = 0; j< parentPathList.size(); j++)
				{
					// 最新履歴オブジェクトが重複し、かつオブジェクトのパスが等しい
					String otherParentPath = (String)parentPathList.get(j);
					if (otherParentPath.compareTo(crntParentPath) == 0)
					{
						return true;
					}
				}
			}
			else
			{
				// ハッシュマップに入れるリストを新しく定義
				parentPathList = new ArrayList();
			}

			parentPathList.add(crntParentPath);
			latestObjMap.put(latestObjIdStr, parentPathList);
		}

		return false;
	}
	
	
	/**
	 * 引数のドキュメントの全ての履歴のリンクリレーションに対して、
	 * リンク更新タイミングが自動、リンク元が最新履歴でない、かつリンク先のフォルダがごみ箱に存在していない場合、
	 * リンクリレーションの子オブジェクト（リンク元）を最新履歴のドキュメントに変更する。
	 * 権限判定は行わない。
	 * 
	 * @param sess EIMSession EIMSession
	 * @param docObj ドキュメントオブジェクト
	 * @throws Exception
	 * @throws EIMException
	 */
	static public void actUpdateObjectLinkByDocument(EIMSession sess, EIMObject docObj)
	throws Exception
	{
		
		// 「リンク」リレーションタイプ
		EIMRelationType linkRelType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_LINK"));
		// 属性タイプ「ドキュメントリンク」
		EIMAttributeType attType_DocLink = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_DOCUMENT_LINK"));
		// 属性タイプ「リンク先」
		EIMAttributeType attType_Link = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
		// 属性タイプ「パス」
		EIMAttributeType attType_Path = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		
		// 最新版へのリンク自動更新実行フラグ（属性値をまとめて更新するために使用）
		boolean isUpdateObjectLinkToLatest = false;
		// 過去履歴へのリンク自動更新実行フラグ（属性値をまとめて更新するために使用）
		boolean isUpdateObjectLinkToOld = false;
		// 同じ格納先に最新版へのリンクリレーションが重複しなようにするためのチェック用HashSet
		// 最新版へのリンクリレーションが存在しているフォルダ、またはワークスペースのIDを格納する
		HashSet<String> existsLatestDocLinkSet = new HashSet<String>();
		
		// バージョンを取得
		EIMVersion version = VersionUtils.getVersion(sess, docObj);
		// 最新版のドキュメントを取得
		EIMObject latestObj = version.getLatest();
		// 最新版の「パス」属性値を取得
		String[] latestPathArray = AppObjectUtil.getStrAttrs(sess, latestObj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		// 最新版の「リンク先」属性値を取得
		long[] latestLinkArray = AppObjectUtil.getIntAttrs(sess, latestObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
		
		// 全履歴のドキュメントを取得
		List<EIMObject> objectList = version.getList();
		
		// 最新版ドキュメントへのリンクを取得してチェック用HashSetに追加する
		List<EIMRelation> latestObjLinkRelList = RelationUtils.getParentRelationListByRelType(sess, latestObj, linkRelType);
		for(EIMRelation latestObjLinkRel : latestObjLinkRelList) {
			existsLatestDocLinkSet.add(Long.toString(latestObjLinkRel.getParent().getId()));
		}
		
		// 各履歴に対してリンク自動更新処理を行う（最新履歴は除く）
		for(int revCnt = 0; revCnt < objectList.size() - 1; revCnt++) {
			EIMObject oldObj = objectList.get(revCnt);
			
			// 過去履歴の「リンク先」属性値
			long[] oldLinkArray = AppObjectUtil.getIntAttrs(sess, oldObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK"));
			// 過去履歴の「パス」属性値
			String[] oldPathArray = AppObjectUtil.getStrAttrs(sess, oldObj, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
			// 過去履歴のドキュメントリンク存在フラグ（true:有、false:無）
			boolean existsOldDocumentLink = true;
			
			// ドキュメントリンクを取得
			List<EIMRelation> linkRelList = RelationUtils.getParentRelationListByRelType(sess, oldObj, linkRelType);
			for(EIMRelation linkRel : linkRelList) {
				// 対象の履歴に対して作成されているドキュメントリンクの数だけ以降の処理を行う
				// リンク自動更新処理は過去履歴を指し示しているドキュメントリンクを最新版を指し示すように
				// リレーションの張り替えを行う他に、
				// 過去履歴・最新版の「パス」・「リンク先」・「ドキュメントリンク」オブジェクト属性も更新する必要がある
				
				// リンク格納先のフォルダ、またはワークスペースのオブジェクト
				EIMObject fwObj = linkRel.getParent();
				// リンク格納先のフォルダ、またはワークスペースのオブジェクトID
				long fwObjId = fwObj.getId();
				// リンク格納先のフォルダ、またはワークスペースの「パス」属性値
				String linkPathStr = null;
				
				// リンク更新タイミングが0:手動の場合は以降の処理は行わない
				if(AppConstant.LINK_UPDATE_TIMING_MANUAL == (linkRel.getAttributeByName(EIMConfig.get("ATTR_NAME_LINK_UPDATE_TIMING"))).getInt()) continue;
				
				// リンク先のフォルダがごみ箱に存在している場合は以降の処理は行わない
				if(AppObjectUtil.isObjectInRecycleWithoutRecycle(sess, fwObj)) continue;
				
				// 最新版へのリンクが格納先に存在しているのかをチェックする
				if(!existsLatestDocLinkSet.contains(Long.toString(fwObj.getId()))) {
					// 存在していない場合
					
					// 過去履歴のドキュメントリンクを削除する
					if( ( oldLinkArray != null ) && ( oldPathArray != null ) && ( (oldLinkArray.length + 1) == oldPathArray.length ) ) {
						
						long[] tmpOldLinkArray = new long[oldLinkArray.length];
						String[] tmpOldPathArray = new String[oldPathArray.length];
						// 最初の「パス」はドキュメントのパス
						tmpOldPathArray[0] = oldPathArray[0];
						int len = 0;
						// 過去履歴の「パス」・「リンク先」属性値の配列から、削除するドキュメントリンクが存在している
						// フォルダ、またはワークスペースのオブジェクトIDを取り除いて一時配列に格納する
						for( int i = 0; i < oldLinkArray.length; i++ ) {
							if( oldLinkArray[i] != fwObjId ) {
								tmpOldLinkArray[len] = oldLinkArray[i];
								tmpOldPathArray[len + 1] = oldPathArray[i + 1];
								len++;
							} else {
								linkPathStr = oldPathArray[i + 1];
							}
						}
						if( len == 0 ) {
							// 削除後、ドキュメントリンクが存在しない場合
							// 過去履歴のドキュメントリンク存在フラグを無に設定する
							existsOldDocumentLink = false;
						}
						else {
							// 削除後、ドキュメントリンクが存在する場合
							// 「リンク先」属性値の配列に削除後の状態の配列を設定する
							oldLinkArray = new long[len];
							System.arraycopy(tmpOldLinkArray, 0, oldLinkArray, 0, len);
							// 過去履歴のドキュメントリンク存在フラグを有に設定する
							existsOldDocumentLink = true;
						}
						// 「パス」属性値の配列に削除後の状態の配列を設定する
						oldPathArray = new String[len + 1];
						System.arraycopy(tmpOldPathArray, 0, oldPathArray, 0, len + 1);
					}
					
					
					// 最新版にドキュメントリンクを追加する
					String[] tmpLatestPathArray = null;
					if( latestPathArray != null ) {
						// 「パス」属性値の一時配列に追加されたドキュメントリンクが存在している
						// フォルダ、またはワークスペースのパス文字列を追加する
						tmpLatestPathArray = new String[latestPathArray.length + 1];
						System.arraycopy(latestPathArray, 0, tmpLatestPathArray, 0, latestPathArray.length);
						tmpLatestPathArray[latestPathArray.length] = linkPathStr;
					}
					else {
						// 「パス」属性値の一時配列に追加されたドキュメントリンクが存在している
						// フォルダ、またはワークスペースのパス文字列を追加する
						tmpLatestPathArray = new String[1];
						tmpLatestPathArray[0] = linkPathStr;
					}
					// 「パス」属性値に追加後の値を設定する
					latestPathArray = new String[tmpLatestPathArray.length];
					System.arraycopy(tmpLatestPathArray, 0, latestPathArray, 0, tmpLatestPathArray.length);
					
					long[] tmpLatestLinkArray = null;
					if( latestLinkArray != null ) {
						// 「リンク先」属性値の一時配列に追加されたドキュメントリンクが存在している
						// フォルダ、またはワークスペースのオブジェクトIDを追加する
						tmpLatestLinkArray = new long[latestLinkArray.length + 1];
						System.arraycopy(latestLinkArray, 0, tmpLatestLinkArray, 0, latestLinkArray.length);
						tmpLatestLinkArray[latestLinkArray.length] = fwObjId;
					}
					else {
						// 「リンク先」属性値の一時配列に追加されたドキュメントリンクが存在している
						// フォルダ、またはワークスペースのオブジェクトIDを追加する
						tmpLatestLinkArray = new long[1];
						tmpLatestLinkArray[0] = fwObjId;
					}
					// 「リンク先」属性値の配列に追加後の状態の配列を設定する
					latestLinkArray = new long[tmpLatestLinkArray.length];
					System.arraycopy(tmpLatestLinkArray, 0, latestLinkArray, 0, tmpLatestLinkArray.length);
					
					// リンクリレーションの子オブジェクトを変更する
					RelationUtils.replaceChildRelation(sess, linkRel, latestObj);
					
					// SearchFramework 検索FW更新通知 対象：旧リンク元、新リンク元
					AppUpdateNoticeUtils.updateNoticeInsert(linkRel.getChild().getId(), "SEARCHFW_UPDATEOBJLINK_DOCUMENTLINK");
					AppUpdateNoticeUtils.updateNoticeInsert(latestObj.getId(), "SEARCHFW_UPDATEOBJLINK_DOCUMENT_LATEST");
					
					// チェック用HashSetにドキュメントリンクが格納されているフォルダ、またはワークスペースのオブジェクトIDを追加する
					existsLatestDocLinkSet.add(Long.toString(fwObj.getId()));
					
					// 最新版へのリンク自動更新実行フラグをtrueに設定する
					isUpdateObjectLinkToLatest = true;
					// 過去履歴へのリンク自動更新実行フラグをtrueに設定する
					isUpdateObjectLinkToOld = true;
				}
			}
			
			// 過去履歴へのリンク自動更新実行フラグがtrueの場合、
			// 配列に設定した「パス」、「リンク先」でDB更新を行う
			if(isUpdateObjectLinkToOld){
				// 「パス」属性値にドキュメントリンク削除後の値を設定する
				ObjectAttributeUtils.setAttribute(sess, oldObj, attType_Path, oldPathArray);
				if(existsOldDocumentLink){
					// 削除後も過去履歴のドキュメントリンクが存在している場合
					
					// 「リンク先」属性値にドキュメントリンクが存在しているフォルダ、またはワークスペースのオブジェクトIDを設定する
					ObjectAttributeUtils.setAttribute(sess, oldObj, attType_Link
							, TypeConvertUtils.convertToBuildTypeArray(oldLinkArray));
					// 「ドキュメントリンク」属性値に1（ドキュメントリンクが存在）を設定する
					ObjectAttributeUtils.setAttribute(sess, oldObj, attType_DocLink, 1);
				}else{
					// 削除後に過去履歴のドキュメントリンクが存在していない場合
					
					// 「リンク先」属性値を削除する
					ObjectAttributeUtils.deleteAttribute(sess, oldObj, attType_Link);
					// 「ドキュメントリンク」属性値に0（ドキュメントリンクが存在しない）を設定する
					ObjectAttributeUtils.setAttribute(sess, oldObj, attType_DocLink, 0);
				}
			}
			// 次の過去履歴のために過去履歴へのリンク自動更新実行フラグをfalseに設定する
			isUpdateObjectLinkToOld = false;
		}
		
		
		// 最新版へのリンク自動更新実行フラグがtrueの場合、
		// 配列に設定した「パス」、「リンク先」でDB更新を行う
		if(isUpdateObjectLinkToLatest){
			// 最新版に対してまとめて更新
			// 「パス」属性値にドキュメントリンク追加後の値を設定する
			ObjectAttributeUtils.setAttribute(sess, latestObj, attType_Path, latestPathArray);
			// 「リンク先」属性値にドキュメントリンクが存在しているフォルダ、またはワークスペースのオブジェクトIDを設定する
			ObjectAttributeUtils.setAttribute(sess, latestObj, attType_Link
					, TypeConvertUtils.convertToBuildTypeArray(latestLinkArray));
			// 「ドキュメントリンク」属性値に1（ドキュメントリンクが存在）を設定する
			ObjectAttributeUtils.setAttribute(sess, latestObj, attType_DocLink, 1);
			// アクセス履歴に登録する
			AccessUtils.createAccess(sess, latestObj, "EIM.ACCESS.TYPE.DOCLINK.UPDATE");
		}
		
	}
	
}
