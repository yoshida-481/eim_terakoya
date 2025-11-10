package common.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import app.document.search.EIMDocSearchType;
import common.bo.TagTreeItem;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelationType;
import eim.bo.EIMResource;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSecurity;
import eim.bo.EIMUser;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.RelationUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * 
 * タグ関連共通クラス
 *
 */
public class TagUtil {
	
	/**
	 * タグ配下の階層構造(タグ付与対象一覧)を返却します。
	 * 
	 * <li>タグ配下のタグは展開しません。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 「タグ」オブジェクト
	 * @return 階層構造(タグ付与対象一覧)
	 * @throws Exception
	 */
	public static TagTreeItem getTagTree(EIMSession sess, EIMObject obj) throws Exception {

		List reservationList = new ArrayList();	// 探索保留リスト(ReservationPathInfosのリスト)
		
		TagTreeItem rootTreeItem = new TagTreeItem(obj, new ArrayList());
		
		// 引数タグの付与されたオブジェクトを全て取得
		List tagGivenObjList = getTagGivenObj(sess, obj);
		
		if (tagGivenObjList != null && tagGivenObjList.size() > 0) {
			
			// 取得EIMObjectをパス探索用PathInfoクラスに格納
			List firstSearchTargetList = new ArrayList();
			for (Iterator iter = tagGivenObjList.iterator(); iter.hasNext();) {
				firstSearchTargetList.add(new PathInfo((EIMObject) iter.next()));
			}
			// 探索保留リストに追加する
			reservationList.add(new ReservationPathInfos(firstSearchTargetList, rootTreeItem, 0));
		
			// 探索保留リスト毎のループ
			while (reservationList.size() > 0) {

				ReservationPathInfos pathInfos = (ReservationPathInfos)reservationList.get(0);
				List searchTargetList = pathInfos.getPathInfoList();	// 本ループの探索対象となるPathInfoのリスト
				reservationList.remove(0);
				
				// 第1個目のパス配列を元に探索
				PathInfo pathInfo = (PathInfo)searchTargetList.get(0);
				String[] path = pathInfo.getPath();
				searchTargetList.remove(0);
				
				TagTreeItem parentItem = pathInfos.getTreeItem();	// 親のTagTreeItem
				TagTreeItem nextParentItem = null;					// 次周の親のTagTreeItem
				
				// パス配列毎のループ
				for (int ii = pathInfos.getRestartLoopNum() ; path.length > ii ; ii++) {	// 前回中断した階層から再開する
					
					List samePathList = new ArrayList();			// 同一パスリスト
					List toReservationList = new ArrayList();		// 探索保留リスト行きのリスト
					
					if (nextParentItem != null) {
						// 前周での「次周の親」を現ループの親に設定
						parentItem = nextParentItem;
						nextParentItem = null;
					}
	
					// 探索対象PathInfo毎のループ
					for (int jj = 0 ; searchTargetList.size() > jj ; jj++) {
						
						PathInfo tmpPathInfo = (PathInfo)searchTargetList.get(jj);
						
						// 比較パスが一致する場合
						if (path[ii].equals(tmpPathInfo.getPath()[ii])){
							
							// パスがここで終了する場合、探索対象PathInfoを当該パスのEIMObjectとして設定
							if (tmpPathInfo.getPath().length - 1 == ii) {
								
								// 親の子として本オブジェクトを設定する
								TagTreeItem currentItem = new TagTreeItem(tmpPathInfo.getEimObject(), null);
								parentItem.addTreeItem(currentItem);
								nextParentItem = currentItem;		// 本オブジェクトを次周の親として設定
								
							} else {
								// 同一パスリストに追加
								samePathList.add(tmpPathInfo);
							}
						
						// 比較パスが不一致の場合
						} else {
							// 探索保留リスト行きのリストに追加
							toReservationList.add(tmpPathInfo);
						}
						
					}// 探索対象PathInfo毎のループ END
					
					// 探索保留リスト行きのリストが1件以上ある場合
					if (toReservationList.size() > 0) {
						// 探索保留リストに追加
						reservationList.add(new ReservationPathInfos(toReservationList, parentItem, ii));	// 本オブジェクトの親の元で、もう一度同じ階層から探らせる
					}
					
					// パス配列ループの最終周の場合 (当該pathInfoが当該パスのEIMObject)
					if (path.length - 1 == ii) {
						
						// 親の子として本オブジェクトを設定
						TagTreeItem currentItem = new TagTreeItem(pathInfo.getEimObject(), null);
						parentItem.addTreeItem(currentItem);
						
						// 同一パスリストは探索保留リストに追加
						if (samePathList.size() > 0) {
							reservationList.add(new ReservationPathInfos(samePathList, currentItem, ii + 1));	// 自分自身が親となり、次の階層から探らせる
						}
						
					} else {
						
						// 既にルートタグ以外の親が見つかっている、かつ、本階層で該当するタグ付与オブジェクトが見つからない場合
						if (parentItem.getEimObject().getId() != rootTreeItem.getEimObject().getId() && nextParentItem == null) {
						
							// 当該パスに該当する補完フォルダ(タグ未割当)を取得
							List objList = getMatchPathObject(sess, pathInfo, ii);
							
							if (objList == null || objList.size() == 0) {
								// 補完フォルダを取得できない(アクセス権限がない)場合、アクセス権限のないフォルダ配下で別途ツリーを構成する
								// (本パスより上に補完フォルダがある場合、空のフォルダとして残る)
								nextParentItem = rootTreeItem;	// 次周の親をルートタグにする
								
							} else {
								// 補完フォルダを親の子として設定
								TagTreeItem dummyItem = new TagTreeItem((EIMObject)objList.get(0), null);
								parentItem.addTreeItem(dummyItem);
								nextParentItem = dummyItem;	// 次周の親として設定
							}
						}
						// 同一パスリストは次周の探索対象にする
						searchTargetList = samePathList;
					}
				}// パス配列毎のループ END
				
			}// 探索保留リスト毎のループ END
		}
		return rootTreeItem;
	}

	/**
	 * タグ配下の階層構造(タグ付与対象一覧)を返却します。
	 * 
	 * <li>タグ配下のタグも展開します。
	 * <li>タグの階層構造でループが存在する場合は例外を発生させます。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 「タグ」オブジェクト
	 * @return 階層構造(タグ付与対象一覧)
	 * @throws Exception
	 */
	public static TagTreeItem getTagTreeWithChild(EIMSession sess, EIMObject obj) throws Exception {
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// ルートタグ配下を取得
		TagTreeItem tagItem = getTagTree(sess, obj);
		
		long[] checkTagIds = {obj.getId()};
		
		// 冗長なタグ取得回避用のマップ [key]オブジェクトID [value]TagTreeItem
		HashMap tagMap = new HashMap();
		tagMap.put(new Long(obj.getId()), tagItem);
		
		// 再帰的に配下のタグ階層構造を取得＆ループチェック
		getTagTreeWithChildRecurrently(sess, tagItem.getTreeItemList(), checkTagIds, tagMap, helper);
		
		return tagItem;
	}
	
	/**
	 * (再帰的処理)配下リストを辿って、タグがある場合はタグ配下も取得します。
	 * 
	 * <li>タグの階層構造でループが存在する場合は例外を発生させます。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param tagTreeItemList 配下のタグツリー構成要素のリスト
	 * @param checkTagIds 配下でチェックに必要なタグID配列
	 * @param tagMap 冗長なタグ取得回避用のマップ [key]オブジェクトID [value]TagTreeItem
	 * @param helper 条件判定ヘルパー
	 * @throws Exception
	 */
	private static void getTagTreeWithChildRecurrently(EIMSession sess, List tagTreeItemList, long[] checkTagIds, 
			HashMap tagMap, AppObjectConditionHelper helper) throws Exception {
		
		if (tagTreeItemList != null && tagTreeItemList.size() > 0) {
			
			// ツリー構成要素リストの分だけループ
			for (Iterator iter = tagTreeItemList.iterator(); iter.hasNext();) {
				TagTreeItem item = (TagTreeItem) iter.next();
				
				// タグの場合
				if (helper.isTypeOfTag(item.getEimObject().getType())) {
					
					// ループチェック
					long [] nextCheckTagIds = checkTagLoop(sess, checkTagIds, item.getEimObject().getId());
					
					if (tagMap.containsKey(new Long(item.getEimObject().getId()))) {
						// 既に取得済みの場合、再取得せずにそれを設定
						item.setTreeItemList(((TagTreeItem)tagMap.get(new Long(item.getEimObject().getId()))).getTreeItemList());
						
					} else {
						// 当該タグ配下を取得
						TagTreeItem tagItem = getTagTree(sess, item.getEimObject());
						
						// 取得したツリー構成要素で差し替え
						item.setTreeItemList(tagItem.getTreeItemList());
						
						// 冗長なタグ取得回避用のマップに格納
						tagMap.put(new Long(item.getEimObject().getId()), item);
						
						// 再起呼び出し
						getTagTreeWithChildRecurrently(sess, item.getTreeItemList(), nextCheckTagIds, tagMap, helper);
					}
					
				} else {
					// 再起呼び出し
					getTagTreeWithChildRecurrently(sess, item.getTreeItemList(), checkTagIds, tagMap, helper);
				}
			}
		}
	}
	
	/**
	 * タグ配下の階層構造(タグ付与対象一覧)を返却します。
	 * 
	 * <li>返却対象はタグのみです。
	 * <li>タグ配下のタグも展開します。
	 * <li>タグの階層構造でループが存在する場合は例外を発生させます。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 「タグ」オブジェクト
	 * @return 階層構造(タグ付与対象一覧)
	 * @throws Exception
	 */
	public static TagTreeItem getOnlyTagTree(EIMSession sess, 	EIMObject obj) throws Exception {
		
		// 引数タグの付与されたタグを全て取得する
		List tagGivenTagObjList = getTagGivenTagObj(sess, obj);

		TagTreeItem rootTreeItem = new TagTreeItem(obj, objListToItemList(tagGivenTagObjList));
		
		if (tagGivenTagObjList != null && tagGivenTagObjList.size() > 0) {
			
			long[] checkTagIds = {obj.getId()};

			// 冗長なタグ取得回避用のマップ [key]オブジェクトID [value]TagTreeItem
			HashMap tagMap = new HashMap();
			tagMap.put(new Long(obj.getId()), rootTreeItem);
			
			// 再帰的に配下のタグ階層構造を取得＆ループチェック
			getOnlyTagTreeRecurrently(sess, rootTreeItem.getTreeItemList(), checkTagIds, tagMap);
		}
		return rootTreeItem;
	}
	
	/**
	 * (再起処理)タグ配下のタグを取得します。
	 * 
	 * <li>タグの階層構造でループが存在する場合は例外を発生させます。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param tagTreeItemList 配下のタグツリー構成要素のリスト
	 * @param checkTagIds 配下でチェックに必要なタグID配列
	 * @param tagMap 冗長なタグ取得回避用のマップ [key]オブジェクトID [value]TagTreeItem
	 * @throws Exception
	 */
	private static void getOnlyTagTreeRecurrently(EIMSession sess, List tagTreeItemList, long[] checkTagIds, HashMap tagMap) throws Exception {
		
		if (tagTreeItemList != null && tagTreeItemList.size() > 0) {
			
			// ツリー構成要素リストの分だけループ
			for (Iterator iter = tagTreeItemList.iterator(); iter.hasNext();) {
				TagTreeItem item = (TagTreeItem) iter.next();

				// ループチェック
				long [] nextCheckTagIds = checkTagLoop(sess, checkTagIds, item.getEimObject().getId());
				
				if (tagMap.containsKey(new Long(item.getEimObject().getId()))) {
					// 既に取得済みの場合、再取得せずにそれを設定
					item.setTreeItemList(((TagTreeItem)tagMap.get(new Long(item.getEimObject().getId()))).getTreeItemList());
					
				} else {
					// 当該タグの付与されたタグを全て取得する
					List tagGivenTagObjList = getTagGivenTagObj(sess, item.getEimObject());
	
					// 取得したタグ付与EIMObjectをツリー構成要素として設定
					item.setTreeItemList(objListToItemList(tagGivenTagObjList));
					
					// 冗長なタグ取得回避用のマップに格納
					tagMap.put(new Long(item.getEimObject().getId()), item);
					
					// 再起呼び出し
					getOnlyTagTreeRecurrently(sess, item.getTreeItemList(), nextCheckTagIds, tagMap);
				}
			}
		}
	}

	/**
	 * 指定タグの付与された全てのEIMObjectを返却します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 「タグ」オブジェクト
	 * @return 指定タグが付与された全てのEIMObjectのリスト
	 * @throws Exception
	 */
	public static List getTagGivenObj(EIMSession sess, EIMObject obj) throws Exception {
		
		// タグが割り当てられているオブジェクト一覧を取得
		return AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DIPSLAY_TAGLIST, 
				EIMAccessRole.READ, new EIMSearchLimitCountCondition(SearchUtils.NOT_SPECIFIED, false), obj);
	}
	
	/**
	 * 指定タグの付与された「タグ」EIMObjectを返却します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 「タグ」オブジェクト
	 * @return 指定タグが付与された「タグ」EIMObjectのリスト
	 * @throws Exception
	 */
	public static List getTagGivenTagObj(EIMSession sess, EIMObject obj) throws Exception {
		
		// 検索条件ヘルパー生成
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		// 条件(1) 属性「タグ」が<オブジェクトID>
		EIMAttributeType attrTag = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		EIMSearchConditionCompare attrTagCond = h.eq(h.opAnd(), attrTag, obj.getId());
		conds.addCondition(attrTagCond);
		
		// 条件(2) オブジェクトタイプが「タグ」以下
		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_TAG"));
		EIMSearchConditionIn objTypeCond = h.eqObjTypeWithSubClasses(h.opAnd(), objType.getId(), sess);
		objTypeCond.setHighPriority();
		conds.addCondition(objTypeCond);
		
		// 検索条件項目・返却項目指定インスタンスに条件グループを設定
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setCondition(conds);

		// 検索実行
		selectTarget.setRole(EIMAccessRole.READ);
		return SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(SearchUtils.NOT_SPECIFIED, false));
	}

	/**
	 * 指定EIMObjectの上位タグ(タグ付与対象一覧で指定EIMObjectの上位に位置するタグ)を全て返却します。
	 * 
	 * <li>重複する場合(親Aと親Bの付与元が同一タグ)は重複を除去して返却します。
	 * <li>タグの階層構造でループが存在する場合は例外を発生させます。
	 * <li>返却リストに格納されるタグの順番は順不同です。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 上位タグの探索元となるEIMObject
	 * @return 上位タグEIMObjectのリスト
	 * @throws Exception
	 */
	public static List getParentTags(EIMSession sess, EIMObject obj) throws Exception {
		
		List retList = new ArrayList();
		HashMap objMap = new HashMap();	// 上位タグ格納用 [key]オブジェクトID [value]EIMObject
		
		long[] checkTagIds = {obj.getId()};
		
		// 再起的に上位のタグを取得＆ループチェック
		getParentTagsRecurrently(sess, obj, objMap, checkTagIds);
		
		for (Iterator iter = objMap.keySet().iterator(); iter.hasNext();) {
			EIMObject tagObj = (EIMObject)objMap.get((Integer)iter.next());
			retList.add(tagObj);
		}
		return retList;
	}
	
	/**
	 * (再起処理)タグ付与元のタグを取得します。
	 * 
	 * <li>タグの階層構造でループが存在する場合は例外を発生させます。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 上位タグの探索元となるEIMObject
	 * @param objMap 上位タグ格納用HashMap [key]オブジェクトID [value]EIMObject
	 * @param checkTagIds 上位でチェックに必要なタグID配列
	 * @throws Exception
	 */
	private static void getParentTagsRecurrently(EIMSession sess, EIMObject obj, HashMap objMap, long[] checkTagIds) throws Exception {
		
		// 付与元タグのオブジェクトIDを取得
		long[] tagObjIds = AppObjectUtil.getIntAttrs(sess, obj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		
		if (tagObjIds != null) {
			for (int ii = 0 ; ii < tagObjIds.length ; ii++) {
				// 付与元タグを取得
				EIMObject tagObj = ObjectUtils.getObjectById(sess, tagObjIds[ii]);
				
				if (tagObj != null) {
					// ループチェック
					long [] nextCheckTagIds = checkTagLoop(sess, checkTagIds, tagObj.getId());
					
					// 既に取得済みの場合、それ以上追わない
					if (!objMap.containsKey(new Long(tagObj.getId()))) {
						// 付与元タグを保管
						objMap.put(new Long(tagObj.getId()), tagObj);
						
						// 再起呼び出し
						getParentTagsRecurrently(sess, tagObj, objMap, nextCheckTagIds);
					}
				}
			}
		}
	}
	
	/**
	 * 指定タグ配下の各階層において、EIMObjectのオブジェクト名の重複をチェックします。
	 * 
	 * <li>タグ配下のタグは展開しません。(既にチェック済みのため)
	 * <li>名称重複がある場合は例外を返却するため、本メソッドがtrueを返却することはありません。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param tagObj 「タグ」オブジェクト
	 * @return 名称重複がない場合、false
	 * @throws Exception
	 */
	public static boolean isDupObjNameUnderTag(EIMSession sess, EIMObject tagObj) throws Exception {

		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// タグ配下の階層構造を取得
		TagTreeItem rootTreeItem = getTagTree(sess, tagObj);
		
		// 再起的に名称重複チェック
		return isDupObjNameUnderTagRecurrently(sess, rootTreeItem.getTreeItemList(), tagObj, helper);
	}
	
	/**
	 * (再起処理)タグの階層構造で名称重複をチェックします。
	 * 
	 * <li>名称重複がある場合は例外を返却するため、本メソッドがtrueを返却することはありません。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param tagTreeItemList 配下のtagTreeItemのリスト
	 * @param tagObj 指定の「タグ」オブジェクト
	 * @param helper 条件判定ヘルパー
	 * @return 名称重複が存在しない場合、false
	 * @throws Exception
	 */
	private static boolean isDupObjNameUnderTagRecurrently(EIMSession sess, List tagTreeItemList,EIMObject tagObj, AppObjectConditionHelper helper) throws Exception {
		
		if (tagTreeItemList != null) {
			for (int ii = 0 ; tagTreeItemList.size() > ii ; ii++) {
				
				// 1周目のみ当該階層の名称重複チェックを実施
				if (ii == 0) {
					HashMap objMap = new HashMap();	// 同一階層のEIMObject格納Map [Key]オブジェクト名 [Value]EIMObject
					
					for (Iterator iter = tagTreeItemList.iterator(); iter.hasNext();) {
						TagTreeItem item = (TagTreeItem)iter.next();
						
						if (objMap.containsKey(item.getEimObject().getName())) {
							
							// エラーメッセージの作成 - <フォルダ/ドキュメント/タグのラベル>[<パス + オブジェクト名>]
							String path0 = getErrorMessageForDupNameErr(sess, tagObj, helper);
							String path1 = getErrorMessageForDupNameErr(sess, item.getEimObject(), helper);
							EIMObject dupObj = (EIMObject)objMap.get(item.getEimObject().getName());
							String path2 = getErrorMessageForDupNameErr(sess, dupObj, helper);
							
							String[] message = {path0, path1, path2};
							// {0} について、\n{1} と\n{2} が\n同一階層上で名称が重複するため、タグを付与できません。
							throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGN.DOCFOLNAME.SAME", message);
							
						} else {
							objMap.put(item.getEimObject().getName(), item.getEimObject());
						}
					}
				}
				TagTreeItem item = (TagTreeItem)tagTreeItemList.get(ii);
				
				// 再起呼び出し
				isDupObjNameUnderTagRecurrently(sess, item.getTreeItemList(), tagObj, helper);
			}
		}
		return false;
	}
	
	/**
	 * (再起処理)タグツリーの指定の階層を取得します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param item 探索対象のタグツリー構成要素
	 * @param objIdList 指定階層までのオブジェクトIDリスト(タグ直下のオブジェクトから選択対象の順。第1番目が再起処理で今回探索すべき対象)
	 * @return タグツリーの指定の階層に存在するEIMObjectのリスト
	 * @throws Exception
	 */
	public static List getTagTreeForTarget(EIMSession sess, TagTreeItem item, List objIdList) throws Exception {
		
		if (objIdList != null) {
			
			// 探索途中
			if (objIdList.size() > 0) {
				long objId = Long.parseLong((String)objIdList.get(0));
				objIdList.remove(0);
				
				// 指定のオブジェクトIDに該当する要素の子供を辿る
				for (Iterator iter = item.getTreeItemList().iterator(); iter.hasNext();) {
					TagTreeItem childItem = (TagTreeItem) iter.next();
					if (objId == childItem.getEimObject().getId()) {
						// 再起呼び出し
						return getTagTreeForTarget(sess, childItem, objIdList);
					}
				}
			// 探索の最終周
			} else {
				return itemListToObjList(item.getTreeItemList());
			}
		}
		// タグの割当情報が更新されています。\n「最新の情報に更新」を押してください。
		throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGNINFO.UPDATED.RELOAD");
	}
	
	/**
	 * (再起処理)引数タグツリーの全階層をソートします。
	 * 
	 * <li>引数の指定によりドキュメントを除外したタグツリーを返却します。
	 * <li>第1ソートは「タグ、フォルダ、ドキュメント」の順、第2ソートは「オブジェクト名」昇順、第3ソートは「オブジェクトID」昇順とします。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param item ソート対象のタグツリー
	 * @param isWithoutDocument true:ドキュメントを除外したタグツリーを返却、false:ドキュメントも含んだタグツリーを返却
	 * @param helper 条件判定ヘルパー
	 * @throws Exception
	 */
	public static void sortTagTree(EIMSession sess, TagTreeItem item, boolean isWithoutDocument, AppObjectConditionHelper helper) throws Exception {
		
		if (item.getTreeItemList() != null && item.getTreeItemList().size() > 0) {
			
			List tagList = new ArrayList();
			List folList = new ArrayList();
			List docList = new ArrayList();
			
			for (Iterator iter = item.getTreeItemList().iterator(); iter.hasNext();) {
				TagTreeItem childItem = (TagTreeItem) iter.next();
				// 再起呼び出し
				sortTagTree(sess, childItem, isWithoutDocument, helper);
				
				// オブジェクトタイプ毎に仕分け
				if (helper.isTypeOfTag(childItem.getEimObject().getType())) {
					tagList.add(childItem);
				} else if (helper.isTypeOfFolder(childItem.getEimObject().getType())) {
					folList.add(childItem);
				} else if (!isWithoutDocument && helper.isTypeOfDocument(childItem.getEimObject().getType())) {
					docList.add(childItem);
				}
			}
			
			// 各リストをオブジェクト名昇順でソート (オブジェクトIDのソートは取得時に済んでいるはず)
			if (tagList.size() > 1) {
				tagList = AppObjectUtil.getStrSortedList(tagList, "getName", true);
			}
			if (folList.size() > 1) {
				folList = AppObjectUtil.getStrSortedList(folList, "getName", true);
			}
			if (docList.size() > 1) {
				docList = AppObjectUtil.getStrSortedList(docList, "getName", true);
			}
			// 各リストを連結
			tagList.addAll(folList);
			tagList.addAll(docList);
			
			// ソートしたリストで差し替え
			item.setTreeItemList(tagList);
		}
	}
	
	/**
	 * 引数オブジェクトID配列の並びに合致するタグツリーを取得します。
	 * 
	 * <li>引数オブジェクトID配列が、タグ配下のタグ以降を示す場合は、タグ配下のタグも取得します。
	 * <li>引数オブジェクトID配列の並びと、取得したタグツリー構成が合致しない場合、例外を発生させます。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param objIds タグツリー(タグ付与対象一覧)のルートタグから選択対象までのオブジェクトIDの並び
	 * @return 階層構造(タグ付与対象一覧)
	 * @throws Exception
	 */
	public static TagTreeItem getTagTreeForTarget(EIMSession sess, long[] objIds) throws Exception {
		
		// 条件判定ヘルパー作成
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		
		// ルートタグの取得
		EIMObject rootTagObj = ObjectUtils.getObjectById(sess, objIds[0]);
		
		if (rootTagObj == null || !SecurityUtils.authorized(sess, rootTagObj, sess.getUser(), EIMAccessRole.READ)) {
			// タグの割当情報が更新されています。
			throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGNINFO.UPDATED");
		}
		
		// ルートタグのツリーを取得
		TagTreeItem rootItem = getTagTree(sess, rootTagObj);
		List nextItemList = rootItem.getTreeItemList();
		
		// オブジェクトID配列と一致するタグツリー構成要素の探索
		int ii = 1;
		while (nextItemList.size() > 0 && objIds.length > ii) {
			
			boolean isSearch = false;
			for (Iterator iter = nextItemList.iterator(); iter.hasNext();) {
				TagTreeItem item = (TagTreeItem) iter.next();
				
				if (item.getEimObject().getId() == objIds[ii]) {
					
					// 該当がタグの場合は更にタグの配下を取得
					if (helper.isTypeOfTag(item.getEimObject().getType())) {
						
						// タグツリーの取得
						TagTreeItem tagItem = getTagTree(sess, item.getEimObject());
						
						// 取得したツリーで差し替え
						item.setTreeItemList(tagItem.getTreeItemList());
					}
					// 該当した場合は配下のリストを次の探索対象とする
					nextItemList = item.getTreeItemList();
					ii++;
					isSearch = true;
					break;
				}
			}
			// 見付からない、または、オブジェクトID配列はまだあるのに次の探索対象が存在しない
			if (!isSearch || (nextItemList.size() == 0 && objIds.length > ii)) {
				// タグの割当情報が更新されています。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGNINFO.UPDATED");
			}
		}
		return rootItem;		
	}
	
	/**
	 * 引数PathInfoの指定の階層に合致するEIMObjectを取得します。
	 * 
	 * <li>タグ付与対象一覧で、タグ付与EIMObjectの間の、タグを付与されていないフォルダを補完するために使用します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param pathinfo パス配列とEIMObjectの管理クラス
	 * @param loopNum EIMObjectを取得すべきフォルダの階層。配列の番号で指定。
	 * @return パス条件に合致したEIMObject
	 * @throws Exception
	 */
	private static List getMatchPathObject(EIMSession sess, PathInfo pathinfo, int loopNum) throws Exception {
		
		// 検索条件ヘルパー生成
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		// 検索条件オブジェクト名
		String objName = pathinfo.getPath()[loopNum];
		
		// 条件(1) オブジェクト名が検索条件「オブジェクト名」
		EIMAttributeType fieldOfObjName = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME;
		EIMSearchConditionCompare objNameCond = h.eq(h.opAnd(), fieldOfObjName, objName);
		conds.addCondition(objNameCond);
		
		// 検索条件「パス」
		String path = "/";
		for (int i = 0; i < loopNum; i++) {
			path += pathinfo.getPath()[i] + "/";
		}

		// 条件(2) 属性「パス」が検索条件「パス」
		EIMAttributeType attrPath = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_FOLDER_PASS"));
		EIMSearchConditionCompare attrPathCond = h.eq(h.opAnd(), attrPath, path);
		conds.addCondition(attrPathCond);
		
		// 検索条件項目・返却項目指定インスタンスに条件グループを設定
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setCondition(conds);

		// 検索実行
		selectTarget.setRole(EIMAccessRole.READ);
		return SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(SearchUtils.NOT_SPECIFIED, false));
	}
	
	/**
	 * タグのループチェック処理を行い、更なる配下でチェックに必要なタグID配列を返却します。
	 * 
	 * <li>「チェック対象タグID」が「タグID配列」に含まれる場合は例外を返却します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param checkTagIds タグID配列
	 * @param targetTagId チェック対象タグID
	 * @return 配下でチェックに必要なタグID配列
	 * @throws Exception
	 */
	private static long[] checkTagLoop(EIMSession sess, long[] checkTagIds, long targetTagId) throws Exception {
		
		long retTagIds[] = new long[checkTagIds.length + 1];
		
		for (int ii = 0 ; checkTagIds.length > ii ; ii++) {
			if (targetTagId == (retTagIds[ii] = checkTagIds[ii])) {
				// タグの階層構造内にタグ自身を含むタグ付与はできません。
				throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGN.TAG.LOOPED");
			}
		}
		// 自分自身を以降のチェック対象に追加
		retTagIds[retTagIds.length - 1] = targetTagId;
		
		return retTagIds;
	}
	
	/**
	 * EIMObjectのリストをTagTreeItemのリストに変換します。
	 * 
	 * @param objList EIMObjectのリスト
	 * @return TagTreeItemのリスト
	 */
	private static List objListToItemList(List objList) {
		
		List itemList = new ArrayList();
		if (objList != null) {
			for (Iterator iter = objList.iterator(); iter.hasNext();) {
				itemList.add(new TagTreeItem((EIMObject) iter.next(), null));
			}
		}
		return itemList;
	}
	
	/**
	 * TagTreeItemのリストをEIMObjectのリストに変換します。
	 * 
	 * @param itemList TagTreeItemのリスト
	 * @return EIMObjectのリスト
	 */
	private static List itemListToObjList(List itemList) {
		
		List objList = new ArrayList();
		if (itemList != null) {
			for (Iterator iter = itemList.iterator(); iter.hasNext();) {
				TagTreeItem item = (TagTreeItem) iter.next();
				objList.add(item.getEimObject());
			}
		}
		return objList;
	}
	
	/**
	 * オブジェクトの種類を表すラベル(ドキュメント/フォルダ/タグ)を返却します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 対象オブジェクト
	 * @param helper 条件判定ヘルパー
	 * @return ラベル(ドキュメント/フォルダ/タグ)
	 * @throws Exception
	 */
	private static String getObjectKindLabel(EIMSession sess, EIMObject obj, AppObjectConditionHelper helper) throws Exception {
		
		if (helper.isTypeOfDocument(obj.getType())) {
			return EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.DOCUMENT");	// ドキュメント
		} else if (helper.isTypeOfFolder(obj.getType())) {
			return EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.FOLDER");		// フォルダ
		} else if (helper.isTypeOfTag(obj.getType())) {
			return EIMResource.getMessage(sess, "EIM.LABEL.OBJECTTYPE.TAG");		// タグ
		}
		return "";	// 通常はここまで来ない
	}

	/**
	 * 名称重複チェックのエラーメッセージを作成します。<br>
	 * →<フォルダ/ドキュメント/タグのラベル>[<パス + オブジェクト名>]<br>
	 * ※ 本関数はZIPダウンロードでも使用されます。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param obj 対象オブジェクト
	 * @param helper 条件判定ヘルパー
	 * @return エラーメッセージ
	 * @throws Exception
	 */
	public static String getErrorMessageForDupNameErr(EIMSession sess, EIMObject obj, AppObjectConditionHelper helper) throws Exception {
		
		return getObjectKindLabel(sess, obj, helper) + "[" + AppObjectUtil.getPath(obj) + obj.getName() + "]";
	}

	/**
	 * インナークラス : パス配列とEIMObjectを保持します。
	 * 
	 * <li>オブジェクト名称もパスとして保持します。
	 */
	private static class PathInfo {
		
		/**
		 * パス属性を持つEIMObjectを格納するフィールドです。
		 */
		private EIMObject _eimObject = null;
		
		/**
		 * 「パス属性 + 名称」を"/"で分割した配列を格納するフィールドです。
		 * 
		 * <li>オブジェクト名称もパスとして保持します。
		 */
		private String[] _path = null;
		
		/**
		 * コンストラクタ
		 * 
		 * @param eimObject パス属性を持つEIMObject
		 */
		public PathInfo(EIMObject eimObject) {
			this._eimObject = eimObject;
			String path = AppObjectUtil.getPath(eimObject) + eimObject.getName();	// 名称をパスに含める
			if (path != null) {
				path = path.substring(1);	// 前方の"/"は除去する
				this._path = path.split("/");
			}
		}
		
		/**
		 * パス属性を持つEIMObjectを取得します。
		 * @return パス属性を持つEIMObject
		 */
		public EIMObject getEimObject() {
			return this._eimObject;
		}
		
		/**
		 * 「パス属性 + 名称」を"/"で分割した配列を取得します。
		 * @return 「パス属性 + 名称」を"/"で分割した配列
		 */
		public String[] getPath() {
			return this._path;
		}
	}
	
	/**
	 * インナークラス : 探索保留中のPathInfoを格納します。
	 */
	private static class ReservationPathInfos {
		
		/**
		 * 探索保留中のPathInfoのリストを格納するフィールドです。
		 */
		private List _pathInfoList = null;
		
		/**
		 * 探索保留中のPathInfoの直近の親EIMObjectを保持するTagTreeItemを格納するフィールドです。
		 */
		private TagTreeItem _treeItem = null;
		
		/**
		 * 探索を再開するパスの階層番号を格納するフィールドです。
		 */
		private int _restartLoopNum = 0;
		
		/**
		 * コンストラクタ
		 * 
		 * @param pathInfoList 探索保留中のPathInfoのリスト
		 * @param treeItem 探索保留中のPathInfoの直近の親EIMObjectを保持するTagTreeItem
		 * @param restartLoopNum 探索を再開するパスの階層番号
		 */
		public ReservationPathInfos(List pathInfoList, TagTreeItem treeItem, int restartLoopNum) {
			this._pathInfoList = pathInfoList;
			this._treeItem = treeItem;
			this._restartLoopNum = restartLoopNum;
		}
		
		/**
		 * 探索保留中のPathInfoのリストを取得しまする
		 * @return 探索保留中のPathInfoのリスト
		 */
		public List getPathInfoList() {
			return this._pathInfoList;
		}
		
		/**
		 * 探索保留中のPathInfoの直近の親EIMObjectを保持するTagTreeItemを取得します。
		 * @return 探索保留中のPathInfoの直近の親EIMObjectを保持するTagTreeItem
		 */
		public TagTreeItem getTreeItem() {
			return this._treeItem;
		}
		/**
		 * 探索を再開するパスの階層番号を取得します。
		 * @return 探索を再開するパスの階層番号
		 */
		public int getRestartLoopNum() {
			return _restartLoopNum;
		}
	}
	
	/**
	 * 引数で指定された情報に基づいてタグオブジェクトを作成する。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param tagType 作成するタグのタイプ
	 * @param parentObj 作成するタグの親オブジェクト
	 * @param tagName 作成するタグの名称
	 * @param user タグを作成するユーザー
	 * @return 作成したタグオブジェクト
	 * @throws Exception
	 */	
	public static EIMObject createTag(	EIMSession sess, EIMObjectType tagType, EIMObject parentObj, String tagName, EIMUser user) throws Exception
	{
		// Windows禁止文字チェック
		AppObjectUtil.checkValidateFName(sess, tagName);
		
		//パスの取得
		String path = AppObjectUtil.getPath(parentObj);
		if(path == null) {
			// ワークスペースの場合、パス属性の値を保持していない
			path = "/";
		}
		path += parentObj.getName() + "/";
		
		//親オブジェクトがゴミ箱の下に移動していないかのチェック
		if (AppObjectUtil.isPathInRecycle(path)) {
			throw new EIMException(sess, "EIM.ERROR.INPUT.CREATETAGINRECYCLE");
		}
		
		//ワークフロー付きフォルダ下の場合は、タグを作成できない
		AppObjectConditionHelper helper = new AppObjectConditionHelper(sess);
		// ワークフロー付フォルダ配下にワークフロー付フォルダタイプを指定した場合
		if ((helper.isTypeOfFolderWithWorkflow(parentObj) ||
				helper.isTypeOfFolderUnderFolderWithWorkflow(parentObj))) {
			throw new EIMException(sess, "EIM.ERROR.LOGIC.CANTCREATE.UNDERWF.TAG");
		}
		
		//タグオブジェクトの作成
		EIMObject object = ObjectUtils.createObject(sess, tagType, tagName);
		
		//親オブジェクトとのリレーションを作成
		EIMRelationType relType = RelationUtils.getRelationTypeByName(sess, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));
		RelationUtils.createRelation(sess, relType, parentObj, object,  EIMConstant.DEPU_CHECK_NAME_REV);
		
		//パスを設定
		AppObjectUtil.setPath(sess, object, path);

		EIMAttributeType attType = null;
		//作成者を設定
		if( user != null ) {
			attType = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE"));
			ObjectAttributeUtils.setAttribute(sess, object, attType, user.getId());
		}

		//セキュリティを設定
		EIMSecurity sec = parentObj.getSecurity();
		if(sec != null)
		{
			SecurityUtils.setSecurity(sess, object, sec);
		}
		
		return object;
	}

	/**
	 * 引数で指定されたオブジェクトにタグを付与する。
	 * 
	 * <li>フォルダ配下のドキュメント・フォルダ・タグについても再帰的にタグ付与を行う
	 * <li>書込権限のないオブジェクトに対しても、systemユーザセッションでタグ付与を実施する
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param targetObj タグ付与対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @param user タグを付与したユーザー
	 * @param attrTypeMap 属性タイプの冗長な取得回避用Map [key]属性タイプ名に合致する定義ファイルのID [value]属性タイプ
	 * @param helper 条件判定ヘルパー
	 * @param sessHelper セッションヘルパー
	 * @return 署名・暗号化未実施のドキュメント/タグにタグを付与したかどうか
	 * @throws Exception
	 */	
	public static boolean assignTag(EIMSession sess, EIMObject targetObj, EIMObject tagObj, EIMUser user, HashMap attrTypeMap, AppObjectConditionHelper helper) throws Exception
	{
		boolean flag = false;
		flag = assignTagRecursive(sess, targetObj, tagObj, user, true, true, attrTypeMap, helper);
		setUpdateNoticeTag(sess, targetObj, tagObj);
		return flag;
		
	}

	/**
	 * 引数で指定されたオブジェクトにタグを付与する。
	 * 
	 * <li>書込権限のないオブジェクトに対しても、systemユーザセッションでタグ付与を実施する
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param targetObj タグ付与対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @param user タグを付与したユーザー
	 * @param attrTypeMap 属性タイプの冗長な取得回避用Map [key]属性タイプ名に合致する定義ファイルのID [value]属性タイプ
	 * @param helper 条件判定ヘルパー
	 * @param sessHelper セッションヘルパー
	 * @param isFolderRecursive 「true」ならフォルダ配下のドキュメント・フォルダ・タグについても再帰的にタグ付与を行う
	 * @return 署名・暗号化未実施のドキュメント/タグにタグを付与したかどうか
	 * @throws Exception
	 */	
	public static boolean assignTag(EIMSession sess, EIMObject targetObj, EIMObject tagObj, EIMUser user, HashMap attrTypeMap, AppObjectConditionHelper helper, boolean isFolderRecursive) throws Exception
	{
		boolean flag = false;
		flag = assignTagRecursive(sess, targetObj, tagObj, user, isFolderRecursive, true, attrTypeMap, helper);
		setUpdateNoticeTag(sess, targetObj, tagObj);
		return flag;

	}

	/**
	 * 引数で指定されたオブジェクトにタグを付与する。(内部再帰処理)
	 * 
	 * <li>書込権限のないオブジェクトに対しても、systemユーザセッションでタグ付与を実施する
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param targetObj タグ付与対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @param user タグを付与したユーザー
	 * @param isFolderRecursive 「true」ならフォルダ配下のドキュメント・フォルダ・タグについても再帰的にタグ付与を行う
	 * @param isTop 「true」の場合はassignTag()に渡されたオブジェクトについてタグ付与を実施中
	 *               「false」の場合は子オブジェクトについてタグ付与を実施中
	 * @param attrTypeMap 属性タイプの冗長な取得回避用Map [key]属性タイプ名に合致する定義ファイルのID [value]属性タイプ
	 * @param helper AppObjectConditionHelperクラスのインスタンス
	 * @param sessHelper セッションヘルパー
	 * @return 署名・暗号化未実施のドキュメント/タグにタグを付与したかどうか
	 * @throws Exception
	 */	
	private static boolean assignTagRecursive(EIMSession sess, EIMObject targetObj, EIMObject tagObj, EIMUser user, boolean isFolderRecursive, 
			boolean isTop, HashMap attrTypeMap, AppObjectConditionHelper helper) throws Exception
	{
		//ErrorCheck
		// アクセス権限
		if (!SecurityUtils.authorized(sess, targetObj, user, EIMAccessRole.READ)) {
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NOTAGASSIGNROLE");
		}
		
		//タグ付与対象オブジェクトがゴミ箱の下に移動していないかのチェック
		if (AppObjectUtil.isObjectInRecycleLite(targetObj)) {
			throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGN.RECYCLED");
		}
		boolean isFolder = false;
		boolean haveNotSignedDocTag = false;
		EIMObjectType objType = targetObj.getType();
		// ドキュメントタイプである場合
		if(helper.isTypeOfDocument(objType)) {
			//ステータスのチェックを行う
			long stsKind = targetObj.getStatus() != null ? targetObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			if( stsKind != AppConstant.STATUS_TYPE_KIND_ID_NONE && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC )
			{
				throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGN.DOC.CANTASSIGN.WITHPATH", new Object[]{ getErrorMessageForDupNameErr(sess, targetObj, helper) });
			}
			//「結合処理失敗」ドキュメントかどうかチェックを行う
			if( AppObjectUtil.getIntAttr(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_JOIN_FAIL"), 0) == AppConstant.FLAG_ON ) {
				throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGN.DOC.CANTASSIGN.WITHPATH", new Object[]{ getErrorMessageForDupNameErr(sess, targetObj, helper) });
			}
		}
		// フォルダタイプである場合
		else if(helper.isTypeOfFolder(objType)) {
			isFolder = true;
			long stsKind = targetObj.getStatus() != null ? targetObj.getStatus().getType().getKind() : AppConstant.STATUS_TYPE_KIND_ID_NONE;
			if( stsKind != AppConstant.STATUS_TYPE_KIND_ID_NONE && stsKind != AppConstant.STATUS_TYPE_KIND_ID_PUBLIC )
			{
				throw new EIMException(sess, "EIM.ERROR.LOGIC.TAGASSIGN.FOL.CANTASSIGN.WITHPATH", new Object[]{ getErrorMessageForDupNameErr(sess, targetObj, helper) });
			}
		}
		
		boolean bExist = false;
		//「タグ」属性の重複チェック
		//タグ付与対象のオブジェクトから「タグ」属性を取得
		long[] tagArray = AppObjectUtil.getIntAttrs(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		if(tagArray != null) {
			for(int i=0; i<tagArray.length; i++) {
				if(tagArray[i] == tagObj.getId()) {	// 重複タグのチェック
					bExist = true;
					break;
				}
			}
		}
		
		//「タグ」属性の設定
		if(!bExist) {
			long[] newTagArray = null;
			if(tagArray != null) {
				newTagArray = new long[tagArray.length + 1];
				System.arraycopy(tagArray, 0, newTagArray, 0, tagArray.length);
				newTagArray[tagArray.length] = tagObj.getId();
			}
			else {	// 新規登録
				newTagArray = new long[1];
				newTagArray[0] = tagObj.getId();
			}
			ObjectAttributeUtils.setAttribute(sess, targetObj, (EIMAttributeType)attrTypeMap.get("ATTR_NAME_DOCUMENT_TAG")
					, TypeConvertUtils.convertToBuildTypeArray(newTagArray));
		}
		//「タグ付与者」属性の設定
		if(!bExist) {
			long[] userArray = AppObjectUtil.getIntAttrs(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVER"));
			long[] newUserArray = null;
			if(userArray != null) {
				newUserArray = new long[userArray.length + 1];
				System.arraycopy(userArray, 0, newUserArray, 0, userArray.length);
				newUserArray[userArray.length] = user.getId();
			}
			else {
				newUserArray = new long[1];
				newUserArray[0] = user.getId();
			}
			ObjectAttributeUtils.setAttribute(sess, targetObj, (EIMAttributeType)attrTypeMap.get("ATTR_NAME_DOCUMENT_TAG_GIVER")
					, TypeConvertUtils.convertToBuildTypeArray(newUserArray));
		}
		//「タグ付与日」属性の設定
		if(!bExist) {
			Date[] dateArray = AppObjectUtil.getDateAttrs(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVENDATE"));
			Date[] newDateArray = null;
			if(dateArray != null) {
				newDateArray = new Date[dateArray.length + 1];
				System.arraycopy(dateArray, 0, newDateArray, 0, dateArray.length);
				newDateArray[dateArray.length] = new Date();	//現在時刻を格納
			}
			else {
				newDateArray = new Date[1];
				newDateArray[0] = new Date();
			}
			ObjectAttributeUtils.setAttribute(sess, targetObj, (EIMAttributeType)attrTypeMap.get("ATTR_NAME_DOCUMENT_TAG_GIVENDATE"), newDateArray);
		}
		
		// アクセス履歴作成
		if(!bExist)
			AccessUtils.createAccess(sess, targetObj, "EIM.ACCESS.TYPE.TAG.ASSIGN");
		
		// 署名・暗号化対象のドキュメント、または、タグか？
		if ( (helper.isTypeOfDocument(targetObj.getType()) && SignUtil.isSignEncrTarget(sess, targetObj))
			|| (helper.isTypeOfTag(targetObj.getType())) ){
			
			long signencr = AppObjectUtil.getIntAttr(sess, targetObj, helper.getAttrNameOfSignEncStatus(),
													AppConstant.SIGNENCR_KIND_NOT_SIGNENCR);
			if (signencr != AppConstant.SIGNENCR_KIND_SIGNENCR) {
				// 署名・暗号化未実施
				haveNotSignedDocTag = true;
			}
		}
		
		//タグ付与対象オブジェクトがフォルダである場合は配下オブジェクトについて再帰呼び出し
		if( isFolderRecursive && isFolder ) {
			// 配下のドキュメント・フォルダ・ドキュメントリンクのリンク元ドキュメントを取得
			HashMap<EIMObject, String> isDocumentLinkMap = new HashMap<EIMObject, String>();
			List childObjs = helper.getChildObjectsWithDocLinkInAccessibleStatus(targetObj, isDocumentLinkMap);
			for (Iterator i = childObjs.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				boolean isDocumentLinkFlag = (isDocumentLinkMap.get(tmpObj).equals("true")) ? true : false;
				haveNotSignedDocTag = haveNotSignedDocTag | assignTagRecursive(sess, tmpObj, tagObj, user, isFolderRecursive, false, attrTypeMap, helper);

				// 更新通知
				setUpdateNoticeTagChild(sess, tmpObj, isDocumentLinkFlag);
			}
		}
		return haveNotSignedDocTag;
	}
	
	/**
	 * (冗長な取得回避用)タグ、タグ付与者、タグ付与日の属性タイプを格納したHashMapを取得します。
	 * 
	 * @param sess EIMSessionインスタンス
	 * @return 属性を格納したHashMap。 [key]属性名に合致する定義ファイルのID [value]属性タイプ
	 * @throws Exception
	 */
	public static HashMap getTagAttrTypeMap(EIMSession sess) throws Exception {
		
		HashMap map = new HashMap();
		map.put("ATTR_NAME_DOCUMENT_TAG", AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG")));						// タグ
		map.put("ATTR_NAME_DOCUMENT_TAG_GIVER", AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVER")));			// タグ付与者
		map.put("ATTR_NAME_DOCUMENT_TAG_GIVENDATE", AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVENDATE")));	// タグ付与日
		
		return map;
	}

	/**
	 * 引数で指定されたオブジェクトからタグを解除する。actAddTagA.jsp用
	 * 
	 * <li>書込権限のないオブジェクトに対しても、systemユーザセッションでタグ解除を実施する
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param targetObj タグ解除対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @param sessHelper セッションヘルパー
	 * @param isFolderRecursive 「true」ならフォルダ配下のドキュメント・フォルダ・タグについても再帰的にタグ解除を行う
	 * @throws Exception
	 */	
	public static void removeTag(EIMSession sess, EIMObject targetObj, EIMObject tagObj, boolean isFolderRecursive, HashMap<Long, EIMObject> remveObjMap) throws Exception
	{
		AppObjectConditionHelper _helper = new AppObjectConditionHelper(sess);
		removeTagRecursive(sess, targetObj, tagObj, isFolderRecursive, _helper, remveObjMap);

	}
	
	/**
	 * 引数で指定されたオブジェクトからタグを解除する。
	 * 
	 * <li>書込権限のないオブジェクトに対しても、systemユーザセッションでタグ解除を実施する
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param targetObj タグ解除対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @param sessHelper セッションヘルパー
	 * @param isFolderRecursive 「true」ならフォルダ配下のドキュメント・フォルダ・タグについても再帰的にタグ解除を行う
	 * @throws Exception
	 */	
	public static void removeTag(EIMSession sess, EIMObject targetObj, EIMObject tagObj, boolean isFolderRecursive) throws Exception
	{
		AppObjectConditionHelper _helper = new AppObjectConditionHelper(sess);
		removeTagRecursive(sess, targetObj, tagObj, isFolderRecursive, _helper, null);

	}
	
	/**
	 * 引数で指定されたオブジェクトからタグを解除する。
	 * ラッパークラス
	 * (内部再帰処理)
	 * 
	 * <li>書込権限のないオブジェクトに対しても、systemユーザセッションでタグ解除を実施する
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param targetObj タグ解除対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @param isFolderRecursive 「true」ならフォルダ配下のドキュメント・フォルダ・タグについても再帰的にタグ解除を行う
	 * @param helper AppObjectConditionHelperクラスのインスタンス
	 * @param sessHelper セッションヘルパー
	 * @throws Exception
	 */
	private static void removeTagRecursive(EIMSession sess, EIMObject targetObj, EIMObject tagObj, 
			boolean isFolderRecursive, AppObjectConditionHelper helper, HashMap<Long, EIMObject> remveObjMap) throws Exception
	{
		removeTagRecursiveInternal(sess, targetObj, tagObj, isFolderRecursive, helper, false, remveObjMap);
		setUpdateNoticeTag(sess, targetObj, tagObj);
		return;
	}
	
	/**
	 * 引数で指定されたオブジェクトからタグを解除する。(内部再帰処理)
	 * 
	 * <li>書込権限のないオブジェクトに対しても、systemユーザセッションでタグ解除を実施する
	 * 
	 * @param sess EIMSessionインスタンス
	 * @param targetObj タグ解除対象のオブジェクト
	 * @param tagObj 付与するタグのオブジェクト
	 * @param isFolderRecursive 「true」ならフォルダ配下のドキュメント・フォルダ・タグについても再帰的にタグ解除を行う
	 * @param helper AppObjectConditionHelperクラスのインスタンス
	 * @param sessHelper セッションヘルパー
	 * @throws Exception
	 */	
	private static void removeTagRecursiveInternal(EIMSession sess, EIMObject targetObj, EIMObject tagObj,
			boolean isFolderRecursive, AppObjectConditionHelper helper, boolean isDocumentLink, HashMap<Long, EIMObject> remveObjMap) throws Exception
	{
		AppObjectConditionHelper _helper = helper;
		if(_helper == null)
			_helper = new AppObjectConditionHelper(sess);
		
		//タグ付与対象のオブジェクトから「タグ」「タグ付与者」「タグ付与日」属性を取得
		long[] tagArray = AppObjectUtil.getIntAttrs(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
		long[] userArray = AppObjectUtil.getIntAttrs(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVER"));
		Date[] dateArray = AppObjectUtil.getDateAttrs(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVENDATE"));

		//この条件に合致しない場合は削除を行わない
		if( tagArray != null && userArray != null && dateArray != null &&
					tagArray.length == userArray.length && userArray.length == dateArray.length ) {
			//削除対象のタグの位置を探す
			int nExistKey = -1;
			for(int i=0; i<tagArray.length; i++) {
				if(tagArray[i] == tagObj.getId()) {
					nExistKey = i;
					break;
				}
			}
			
			if( nExistKey == 0 && tagArray.length == 1 ) {	//最後のタグ属性値
				// 属性の削除
				AppObjectUtil.deleteAttribute(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"));
				AppObjectUtil.deleteAttribute(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVER"));
				AppObjectUtil.deleteAttribute(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVENDATE"));
				
			}
			else if( nExistKey > -1 ) {
				long[] newTagArray = new long[tagArray.length - 1];
				long[] newUserArray = new long[userArray.length - 1];
				Date[] newDateArray = new Date[dateArray.length - 1];
				
				int jj = 0;
				for(int ii=0; ii < tagArray.length; ii++){
					if(ii != nExistKey) {
						newTagArray[jj] = tagArray[ii];
						newUserArray[jj] = userArray[ii];
						newDateArray[jj] = dateArray[ii];
						jj++;
					}
				}
				
				//modified by lin.chen at 2010/03/25 reference #408
				// 属性の再設定
//				AppObjectUtil.setAttr(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"), newTagArray);
//				AppObjectUtil.setAttr(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVER"), newUserArray);
//				AppObjectUtil.setAttr(sess, targetObj, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVENDATE"), newDateArray);
				ObjectAttributeUtils.setAttribute(sess, targetObj, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG"))
						, TypeConvertUtils.convertToBuildTypeArray(newTagArray));
				ObjectAttributeUtils.setAttribute(sess, targetObj, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVER"))
						, TypeConvertUtils.convertToBuildTypeArray(newUserArray));
				ObjectAttributeUtils.setAttribute(sess, targetObj, AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_TAG_GIVENDATE")), newDateArray);
				
			}
			
			// 更新通知用(actAddTagA.jsp)
			if( AppUpdateNoticeUtils.doEntry() && remveObjMap!=null )
			{
				remveObjMap.put((long)targetObj.getId(), targetObj);
			}
			
			// アクセス履歴作成
			AccessUtils.createAccess(sess, targetObj, "EIM.ACCESS.TYPE.TAG.RELEASE");
		}
		
		// フォルダに対して再帰処理を行うのなら
		if( isFolderRecursive && _helper.isTypeOfFolder(targetObj.getType()) ) {
			// 配下のドキュメント・フォルダ・ドキュメントリンクのリンク元ドキュメントを取得
			HashMap<EIMObject, String> isDocumentLinkMap = new HashMap<EIMObject, String>();
			List childObjs = _helper.getChildObjectsWithDocLinkInAccessibleStatus(targetObj, isDocumentLinkMap);

			for (Iterator i = childObjs.iterator(); i.hasNext();) {
				EIMObject tmpObj = (EIMObject) i.next();
				boolean isDocumentLinkFlag = (isDocumentLinkMap.get(tmpObj).equals("true")) ? true : false;
				removeTagRecursiveInternal(sess, tmpObj, tagObj, isFolderRecursive, helper, isDocumentLinkFlag, remveObjMap);
				
				// SearchFramework 検索FW更新通知
				setUpdateNoticeTagChild(sess, tmpObj, isDocumentLinkFlag);
			}
		}
		return;
	}
	
	
	/**
	 * 本メソッドはタグ割当の際に1回のみ呼び出される。
	 */
	private static void setUpdateNoticeTag(EIMSession sess, EIMObject object, EIMObject tagObj) throws Exception {
		
		String pattern = (String)EIMThreadContext.get("SEARCHFW.TAG.PATTERN");
		
		if( !AppUpdateNoticeUtils.doEntry() )
		{
			return;
		}
		
		if(pattern!=null)
		{
			if(pattern.equals("SelectTag") || pattern.equals("SelectTagEx"))
			{
				// 選択タグにドキュメント・フォルダ・タグ割当（外部IFも同じキー）
				// SearchFramework 検索FW更新通知 対象：割当ドキュメント、割当フォルダ、割当タグ
				// 選択タグの更新通知はJSP側(外部IFは呼び出し元)で実施
				AppUpdateNoticeUtils.updateNoticeInsertObject(sess, object, 
						"SEARCHFW_SELECTTAG_ALLOCATE_FOLDER", 
						"SEARCHFW_SELECTTAG_ALLOCATE_DOCUMENT",
						"SEARCHFW_SELECTTAG_ALLOCATE_TAG", null); 
			}
			else if(pattern.equals("SelectDoc"))
			{
				// 選択ドキュメント・フォルダ・タグにタグを付与
				// SearchFramework 検索FW更新通知 対象：選択ドキュメント、フォルダ、タグ
				AppUpdateNoticeUtils.updateNoticeInsert(tagObj.getId(), "SEARCHFW_SELECTDOC_ALLOCATE_TAG");
				AppUpdateNoticeUtils.updateNoticeInsertObject(sess, object, 
						"SEARCHFW_SELECTDOC_FOLDER", 
						"SEARCHFW_SELECTDOC_DOCUMENT",
						"SEARCHFW_SELECTDOC_TAG", null); 
			}
			else if(pattern.equals("actDeleteObject") || pattern.equals("physicalDeleteTag"))
			{
				// 選択タグを削除
				AppUpdateNoticeUtils.updateNoticeInsertObject(sess, object, 
						"SEARCHFW_PHYSICALDEL_SETTAG_FOLDER", 
						"SEARCHFW_PHYSICALDEL_SETTAG_DOCUMENT",
						"SEARCHFW_PHYSICALDEL_SETTAG_TAG", null); 
			}
		}
		
	}
	
	/**
	 * 本メソッドは再帰的に呼び出される。
	 */
	private static void setUpdateNoticeTagChild(EIMSession sess, EIMObject object, boolean isDocumentLink) throws Exception {
		
		String pattern = (String)EIMThreadContext.get("SEARCHFW.TAG.PATTERN");

		if( !AppUpdateNoticeUtils.doEntry() )
		{
			return;
		}
		
		if(pattern!=null)
		{
			if(pattern.equals("SelectTag") || pattern.equals("SelectTagEx"))
			{
				// 選択タグにドキュメント・フォルダ・タグ割当(外部IFも同じキー)
				// SearchFramework 検索FW更新通知 対象：フォルダ配下のフォルダ、ドキュメント、タグ、ドキュメントリンク
				AppUpdateNoticeUtils.updateNoticeInsertObjectLink(sess, object, 
						"SEARCHFW_SELECTTAG_CHILD_FOLDER", 
						"SEARCHFW_SELECTTAG_CHILD_DOCUMENT",
						"SEARCHFW_SELECTTAG_CHILD_TAG", null,
						"SEARCHFW_SELECTTAG_CHILD_DOCUMENTLINK", isDocumentLink); 
			}
			else if(pattern.equals("SelectDoc"))
			{
				// 選択ドキュメント・フォルダ・タグにタグを付与
				// SearchFramework 検索FW更新通知 対象：選択フォルダ配下のフォルダ、ドキュメント、タグ、ドキュメントリンク
				AppUpdateNoticeUtils.updateNoticeInsertObjectLink(sess, object, 
						"SEARCHFW_SELECTDOC_CHILD_FOLDER", 
						"SEARCHFW_SELECTDOC_CHILD_DOCUMENT",
						"SEARCHFW_SELECTDOC_CHILD_TAG", null,
						"SEARCHFW_SELECTDOC_CHILD_DOCUMENTLINK", isDocumentLink); 
				
				
			}
			// actDeleteObject , physicalDeleteTagは本メソッドは通らない
		}
		
	}
}