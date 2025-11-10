		package common.util;

import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;

import app.document.search.EIMDocSearchType;
import common.bo.AttributeTree;
import common.bo.AttributeTreeItem;
import eim.bo.EIMAccessRole;
import eim.bo.EIMAttributeType;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMOtherName;
import eim.bo.EIMRelation;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionLike;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSecurity;
import eim.bo.EIMValueType;
import eim.db.DBUtils;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.EIMUtils;
import eim.util.ObjectUtils;
import eim.util.OperationHistoryUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;


/**
 *
 * 属性タイプツリー関連クラス
 *
 * @version 1.1
 */
public class AttributeTreeUtil {

	/**
	 * 属性ツリーオブジェクトを作成して、属性「属性ツリー分類対象」
	 * を設定します。
	 * @param sess セッション
	 * @param attTreeName 属性ツリー名称
	 * @param type 属性ツリー分類対象
	 * @return 属性ツリーオブジェクト
	 * @throws EIMException 指定した属性ツリーが存在していた場合、
	 * 						 またはストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static AttributeTree createAttributeTree(EIMSession sess,
			String attTreeName, long type) throws Exception
	{

		try {

			//存在チェック
			if (AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREE"), attTreeName) == null)
			{
				//属性ツリーオブジェクト生成
				EIMObject objAttTree = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREE"), attTreeName);

				//属性ツリー分類対象を設定
				AppObjectUtil.setAttr(sess, objAttTree, EIMConfig.get("ATTR_NAME_ATTRTREE_CLASS"), type);

				//新規作成のため空のリスト
				List arrayList = new ArrayList();

				//属性ツリー生成
				return new AttributeTree(objAttTree.getId(),
						                  attTreeName,
						                  objAttTree.getName(),
						                  type,
						                  arrayList);
			//存在する場合
			} else {
				throw new EIMException(sess, "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.NAME.EXISTS");
			}

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}


    /**
	 * 属性ツリーオブジェクトをDBから削除します。
	 * @param sess セッション
	 * @param attTree 属性ツリーオブジェクト
	 * @throws EIMException 指定された属性ツリーが存在しない場合、
	 *                       またはストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static void deleteAttributeTree(EIMSession sess,
			AttributeTree attTree) throws Exception
	{
		try {

			//存在チェック
			//存在しない場合
			if (AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREE"), attTree.getDefName()) == null)
			{
				throw new EIMException(sess, "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.NOT.EXIST");
			} else {
				//属性ツリー削除
				AppObjectUtil.deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREE"), attTree.getDefName());

				//削除した属性ツリーに紐つく属性ツリー所属属性を削除
				for (int i = 0; i < attTree.getTreeItemList().size(); i++) {
					AttributeTreeItem attTreeItem = (AttributeTreeItem)attTree.getTreeItemList().get(i);
					EIMObject obj = ObjectUtils.getObjectById(sess, attTreeItem.getId());
					ObjectUtils.deleteObject(sess, obj);
				}

				//削除した属性ツリーに紐つく属性ツリー他言語を削除
				List attrTreeOtherObjList = getOtherAttributeTreeByAttTreeId(sess, attTree.getId());
				for (int i = 0; i < attrTreeOtherObjList.size(); i++) {
					EIMObject obj = (EIMObject)attrTreeOtherObjList.get(i);
					ObjectUtils.deleteObject(sess, obj);
				}
			}

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性ツリーの名称、属性ツリー分類対象を更新します
	 * @param sess セッション
	 * @param attTree 属性ツリーオブジェクト
	 * @param attTreeName 属性ツリー名称
	 * @param type 属性ツリー分類対象
	 * @throws EIMException 指定された属性ツリーが存在しない場合、
	 *                       またはストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static void updateAttributeTree(EIMSession sess,
			AttributeTree attTree, String attTreeName, int type)
	        throws Exception
	{
		try {

			EIMObject objAttTree = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREE"), attTree.getName());

			//存在チェック
			//存在しない場合
			if (objAttTree == null)
			{
				throw new EIMException(sess, "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.NOT.EXIST");
			} else {
				//属性ツリーの名称変更
				objAttTree = ObjectUtils.rename(sess, objAttTree, attTreeName);

				//属性ツリー分類対象更新
				AppObjectUtil.updateAttr(sess, objAttTree, EIMConfig.get("ATTR_NAME_ATTRTREE_CLASS"), type);
			}

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性ツリー他言語オブジェクトを作成して、「属性ツリー他言語ID」、
	 * 「属性ツリー他言語名称」を設定します。
	 * @param sess セッション
	 * @param attTreeId 属性ツリーID（名称）
	 * @param langId 属性ツリー他言語ID
	 * @param attTreeOtherName 属性ツリー他言語名称
	 * @throws EIMException 指定された属性ツリーIDと属性ツリー他言語ID
	 *            　         の属性ツリー他言語オブジェクトが存在する場合、
	 *                       またはストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static void addOtherAttributeTreeName(EIMSession sess,
			long attTreeId, String langId, String attTreeOtherName)
	        throws Exception
	{
		try {
            //存在チェック
			if (getOtherAttributeTreeName(sess, attTreeId, langId) == null) {

				//属性ツリーIDを文字列に変換
				String objName = Long.toString(attTreeId) + "_" + langId;

				//属性ツリー他言語オブジェクト生成
				EIMObject objAttTreeOther = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEOTHER"), objName);

				//属性ツリー他言語IDを設定
				AppObjectUtil.setAttr(sess, objAttTreeOther, EIMConfig.get("ATTR_NAME_ATTRTREEOTHER_LANG_ID"), langId);

				//属性ツリー他言語名称を設定
				AppObjectUtil.setAttr(sess, objAttTreeOther, EIMConfig.get("ATTR_NAME_ATTRTREEOTHER_LANG_NAME"), attTreeOtherName);
			} else {
				throw new EIMException(sess, "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.ALIAS.EXISTS");
			}

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性ツリー他言語オブジェクトを削除します。
	 * @param sess セッション
	 * @param attTreeId 属性ツリーID
	 * @param langId 属性ツリー他言語ID
	 * @throws EIMException 指定された属性ツリーIDと属性ツリー他言語ID
	 *            　         の属性ツリー他言語オブジェクトが存在しない場合、
	 *                       またはストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static void deleteOtherAttributeTreeName(EIMSession sess,
			int attTreeId, String langId) throws Exception
	{
		try {

			//存在チェック
			if (getOtherAttributeTreeName(sess, attTreeId, langId) == null) {
				throw new EIMException(sess, "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.ALIAS.NOT.EXIST");
			} else {
				//属性ツリーIDを文字列に変換(オブジェクト名称)
				String objName = Long.toString(attTreeId) + "_" + langId;

				//属性ツリー他言語削除
				AppObjectUtil.deleteObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEOTHER"), objName);
			}

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性ツリー他言語の「属性ツリー他言語名称」を更新します。
	 * 指定された属性ツリーIDと属性ツリー他言語IDの
	 * 属性ツリー他言語オブジェクトが存在しない場合は
	 * 新規に属性ツリー他言語オブジェクトを作成します。
	 * @param sess セッション
	 * @param attTreeId 属性ツリーID
	 * @param langId 属性ツリー他言語ID
	 * @param attTreeOtherName 属性ツリー他言語名称
	 * @throws EIMException
	 * 				ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static void updateOtherAttributeTreeName(EIMSession sess,
			long attTreeId, String langId, String attTreeOtherName)
			throws Exception
	{
		try {

            //存在チェック
			if (getOtherAttributeTreeName(sess, attTreeId, langId) == null) {
				//新規に属性ツリー他言語オブジェクト生成
				addOtherAttributeTreeName(sess, attTreeId, langId, attTreeOtherName);
			} else {
				//属性ツリーIDを文字列に変換(オブジェクト名称)
				String objName = Long.toString(attTreeId) + "_" + langId;

				//属性ツリー他言語のオブジェクト取得
				EIMObject objAttTreeOther = AppObjectUtil.getObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEOTHER"), objName);

				//属性ツリー他言語名称更新
				AppObjectUtil.updateAttr(sess, objAttTreeOther, EIMConfig.get("ATTR_NAME_ATTRTREEOTHER_LANG_NAME"), attTreeOtherName);
			}
		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性ツリーIDに対応する属性ツリー他言語オブジェクトのリストを取得し、
	 * それをもとにしてEIMOtherNameオブジェクトのリストを生成して返却します。
	 * 属性ツリー他言語オブジェクトが存在しなかった場合は空のリストを返却します。
	 * @param sess セッション
	 * @param attTreeId 属性ツリーID
	 * @return List EIMOtherNameオブジェクトのリスト
	 * @throws EIMException
	 * 				ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static List getOtherAttributeTreeNameList(EIMSession sess,
			long attTreeId) throws Exception
	{
		try {
			// 属性ツリー他言語オブジェクトのリストを取得
			List attrTreeOtherObjList = getOtherAttributeTreeByAttTreeId(sess, attTreeId);

			List resultList = new ArrayList();

			if (attrTreeOtherObjList.size() > 0) {

				//EIMOtherNameのリスト生成
				for (int i = 0; i < attrTreeOtherObjList.size(); i++) {

					EIMObject obj = (EIMObject)attrTreeOtherObjList.get(i);

					EIMOtherName otherName = new EIMOtherName();

					//言語ID設定
					otherName.setLangId(AppObjectUtil.getStrAttr(sess, obj, EIMConfig.get("ATTR_NAME_ATTRTREEOTHER_LANG_ID")));

					//名称設定
					otherName.setName(AppObjectUtil.getStrAttr(sess, obj, EIMConfig.get("ATTR_NAME_ATTRTREEOTHER_LANG_NAME")));

					resultList.add(otherName);
				}
			}

			return resultList;

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性ツリーIDと他言語IDに対応する属性ツリー他言語オブジェクトを取得し、
	 * そのオブジェクトの属性「属性ツリー他言語名称」を返却します。
	 * 属性ツリー他言語オブジェクトを取得できなかった場合はnullを返却します。
	 * @param sess セッション
	 * @param attTreeId 属性ツリーID
	 * @param langId 属性ツリー他言語ID
	 * @return 属性ツリー他言語名称
	 * @throws EIMException
	 * 				ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static String getOtherAttributeTreeName(EIMSession sess,
			long attTreeId, String langId) throws Exception
	{
		try {
			//「属性ツリー他言語」オブジェクトタイプを取得
			EIMObjectType attrTreeOtherObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEOTHER"));

			//検索条件項目・返却項目指定オブジェクトを生成
			EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();

			//検索条件グループ作成
			EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			//条件１:オブジェクトタイプが「属性ツリー他言語」を検索
			EIMAttributeType fieldOfObjType = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE;
			EIMSearchConditionCompare cond1 = new EIMSearchConditionCompare(
					EIMSearchOperatorEnum.AND
					, fieldOfObjType				//TYPE
					, EIMSearchOperatorEnum.EQ		// ==
					, attrTreeOtherObjType.getId() 	//'属性ツリー他言語'
				);
			//条件１を条件グループに加える
			conds.addCondition(cond1);

			//条件２: and 属性「属性ツリー他言語ID」が<入力引数>を検索
			EIMAttributeType attrTreeOtherLangId = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_ATTRTREEOTHER_LANG_ID"));
			EIMSearchConditionCompare cond2 = new EIMSearchConditionCompare(
				EIMSearchOperatorEnum.AND	//前方演算子AND
				, attrTreeOtherLangId		//属性「属性ツリー他言語ID」
				, EIMSearchOperatorEnum.EQ	// ==
				, langId				//入力引数「属性ツリー他言語ID」
			);
			//条件２を条件グループに加える
			conds.addCondition(cond2);

			//属性ツリーIDを文字列に変換(オブジェクト名称)
			String objName = Long.toString(attTreeId) + "_" + langId;

			//条件３: and オブジェクト名が<入力引数>を検索
			EIMAttributeType fieldOfObjName = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME;
			EIMSearchConditionCompare cond3 = new EIMSearchConditionCompare(
				EIMSearchOperatorEnum.AND	//前方演算子AND
				, fieldOfObjName			//オブジェクト名
				, EIMSearchOperatorEnum.EQ	// ==
				, objName				//入力引数「オブジェクト名」
			);
			//条件３を条件グループに加える
			conds.addCondition(cond3);

			//検索条件項目・返却項目指定インスタンスに条件グループを設定
			selectTarget.setCondition(conds);

			//検索実行
			selectTarget.setRole(EIMAccessRole.READ);
			List attrTreeOtherObjList = SearchUtils.searchObjects(sess, selectTarget, null);

			String otherLangName = null;
			if (attrTreeOtherObjList.size() > 0) {
				EIMObject attrTreeOtherObj = (EIMObject)attrTreeOtherObjList.get(0);
				otherLangName = AppObjectUtil.getStrAttr(sess, attrTreeOtherObj, EIMConfig.get("ATTR_NAME_ATTRTREEOTHER_LANG_NAME"));//属性ツリー他言語名称
			}

			return otherLangName;

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 指定された属性ツリーIDに対応する、属性ツリー所属属性情報を全て削除した後、
	 * 属性ツリー所属属性オブジェクトを作成し、「属性ツリー所属属性ID」、
	 * 「所属ツリー所属属性ポジション」、「属性ツリー所属属性値なし表示フラグ」
	 * を設定します。
	 * 属性ツリーアイテム一覧がnullもしくは空の場合、処理を終了します。
	 * @param sess セッション
	 * @param attTree 属性ツリーオブジェクト
	 * @param attTreeItemList 属性ツリーアイテム一覧
	 * @throws EIMException
	 * 				ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static void setTreeItems(EIMSession sess, AttributeTree attTree,
			List attTreeItemList) throws Exception
	{
		try {
			//属性ツリーIDを文字列に変換(オブジェクト名称)
			String objName = Long.toString(attTree.getId());

			//属性ツリーに紐つく属性ツリー所属属性を削除
			for (int i = 0; i < attTree.getTreeItemList().size(); i++) {
				AttributeTreeItem attTreeItem = (AttributeTreeItem)attTree.getTreeItemList().get(i);
				EIMObject obj = ObjectUtils.getObjectById(sess, attTreeItem.getId());
				ObjectUtils.deleteObject(sess, obj);
			}

			if (attTreeItemList == null || attTreeItemList.size() == 0) {
				return;
			} else {

				for (int i = 0; i < attTreeItemList.size(); i++) {

					AttributeTreeItem attTreeItem = (AttributeTreeItem)attTreeItemList.get(i);

					//所属ツリー所属属性オブジェクト生成
					EIMObject objAttTreePos = AppObjectUtil.createObject(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"), objName + "_" + i);

					//属性ツリー所属属性ID設定
					AppObjectUtil.setAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_ATTR_ID"), attTreeItem.getType().getId());

					//属性ツリー所属属性ポジション設定
					AppObjectUtil.setAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_ATTR_POS"), i);

					int intFlag = 0;

					if (attTreeItem.isViewNoValues() == true) {
						intFlag = 1;
					}

					//「属性ツリー所属属性値なし表示フラグ」設定
					AppObjectUtil.setAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_NO_FLG"), intFlag);
				}
			}
		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}

	}

	/**
	 * 属性ツリー所属属性の「属性ツリー所属属性値なし表示フラグ」を更新します。
	 * @param sess セッション
	 * @param attTreeItem 属性ツリーアイテム
	 * @param viewNoValuesFlag 「属性ツリー所属属性値なし表示フラグ」設定値
	 * 							（false:しない /true:する）
	 * @throws EIMException 指定した属性ツリー所属属性が存在しない場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static void updateViewNoValuesFlag(EIMSession sess,
			AttributeTreeItem attTreeItem, boolean viewNoValuesFlag)
			throws Exception
	{
		try {

			// 属性ツリー所属属性オブジェクト取得
			EIMObject objAttrTreePos = ObjectUtils.getObjectById(sess, attTreeItem.getId());

			// 存在チェック
			// 存在しない場合
			if (objAttrTreePos == null)
			{
				throw new EIMException(sess, "EIM.ERROR.COMMON.API.ATTRIBUTE.TREE.ITEM.NOT.EXIST");
			} else {
				int intFlag = 0;

				if (viewNoValuesFlag == true) {
					intFlag = 1;
				}
				//「属性ツリー所属属性値なし表示フラグ」設定
				AppObjectUtil.setAttr(sess, objAttrTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_NO_FLG"), intFlag);
			}

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性ツリーを全て取得し、AttributeTreeのリストに設定して返却します。
	 * 属性ツリーが存在しない場合、空のリストを返却する。
	 * @param sess セッション
	 * @return 属性ツリーのリスト
	 * @throws EIMException
	 * 				ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static List getAttributeTreeList(EIMSession sess) throws Exception
	{

		// 属性ツリー所属属性オブジェクト
		EIMObject objAttTreePos = null;

		// 属性ツリー所属属性オブジェクトのリスト
		List objAttTreePosList = new ArrayList();

		// AttributeTreeのリスト(return値)
		List resultList = new ArrayList();

		// 属性タイプ情報
		EIMAttributeType attType = null;

		// 属性アイテム
		AttributeTreeItem attTreeItem = null;

		try {

			//属性ツリーオブジェクトを全て取得
			//存在しない場合は空のリスト生成
			List objAttTreeList = AppSearchUtils.searchObjectsByConditionMaker(sess, EIMDocSearchType.DISPLAY_ATTRTREE, 
					EIMAccessRole.READ, new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.NOT_SPECIFIED, false), 
					null);

			if (objAttTreeList.size() > 0) {

				//AttributeTreeのリスト生成
				for (int i = 0; i < objAttTreeList.size(); i++) {

					// 属性ツリーオブジェクト取得
					EIMObject obj = (EIMObject)objAttTreeList.get(i);

                    // 「属性ツリー所属属性」オブジェクトのリストを取得
					objAttTreePosList = getAttrTreePosByAttTreeId(sess, obj.getId());

					// 属性アイテムのリスト
					List attTreeItemList = new ArrayList();

					// 属性アイテムのリスト生成
					for (int j = 0; j < objAttTreePosList.size(); j++) {

						// 「属性ツリー所属属性」オブジェクト取得
						objAttTreePos = (EIMObject)objAttTreePosList.get(j);

						// 属性タイプID取得
						long objTypeId = AppObjectUtil.getIntAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_ATTR_ID"), Integer.MIN_VALUE);

						// 属性タイプ情報取得
						attType = AttributeUtils.getAttributeTypeById(sess, objTypeId);

						// 属性ツリー所属属性値なし表示フラグ取得
						long outViewNoValueFlg = AppObjectUtil.getIntAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_NO_FLG"), Integer.MIN_VALUE);
						// 入力値生成
						boolean inputViewNoValueFlag = false;
						if (outViewNoValueFlg == 1) inputViewNoValueFlag = true;

						long position = AppObjectUtil.getIntAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_ATTR_POS"), Integer.MIN_VALUE);

						// 属性アイテム生成
						attTreeItem = new AttributeTreeItem(objAttTreePos.getId(), attType, inputViewNoValueFlag, position);

						attTreeItemList.add(attTreeItem);
					}

					String attTreeOtherName = getOtherAttributeTreeName(sess, obj.getId(), sess.getLangId());

					if (attTreeOtherName == null) {
						attTreeOtherName = obj.getName();
					}

					if (objAttTreePosList.size() > 0) {
						attTreeItemList = AppObjectUtil.getIntSortedList(attTreeItemList, "getPosition", true);
					}

					AttributeTree attTree = new AttributeTree(obj.getId(),
							attTreeOtherName, obj.getName(),
							AppObjectUtil.getIntAttr(sess, obj, EIMConfig.get("ATTR_NAME_ATTRTREE_CLASS"), Integer.MIN_VALUE),
							attTreeItemList);

					resultList.add(attTree);
				}
			}

			return resultList;

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}

	}

	/**
	 * 指定されたIDから属性ツリーを取得し、AttributeTreeに設定して返却します。
	 * 該当する属性ツリーオブジェクトが存在しない場合、nullを返却します。
	 * @param sess セッション
	 * @param attTreeId 属性ツリーID
	 * @return 属性ツリー
	 * @throws EIMException
	 * 				ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static AttributeTree getAttributeTreeById(EIMSession sess,
			long attTreeId)	throws Exception
	{

		List objAttTreePosList = new ArrayList();
		List attTreeItemList = new ArrayList();
		EIMObject objAttTreePos = null;
		EIMAttributeType attType = null;
		AttributeTreeItem attTreeItem = null;

		try {
			//指定されたIDを持つ属性ツリーオブジェクトを取得
			EIMObject objAttTree = ObjectUtils.getObjectById(sess, attTreeId);

			if (objAttTree == null) {
				return null;
			} else {
                // 「属性ツリー所属属性」オブジェクトのリストを取得
				objAttTreePosList = getAttrTreePosByAttTreeId(sess, attTreeId);

				// 属性アイテムのリスト生成
				for (int i = 0; i < objAttTreePosList.size(); i++) {

					// 「属性ツリー所属属性」オブジェクト取得
					objAttTreePos = (EIMObject)objAttTreePosList.get(i);

					// 属性タイプID取得
					long objTypeId = AppObjectUtil.getIntAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_ATTR_ID"), Integer.MIN_VALUE);

					// 属性タイプ情報取得
					attType = AttributeUtils.getAttributeTypeById(sess, objTypeId);

					// 属性ツリー所属属性値なし表示フラグ取得
					long outViewNoValueFlg = AppObjectUtil.getIntAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_NO_FLG"), Integer.MIN_VALUE);
					// 入力値生成
					boolean inputViewNoValueFlag = false;
					if (outViewNoValueFlg == 1) inputViewNoValueFlag = true;

					long position = AppObjectUtil.getIntAttr(sess, objAttTreePos, EIMConfig.get("ATTR_NAME_ATTRTREEPOS_ATTR_POS"), Integer.MIN_VALUE);

					// 属性アイテム生成
					attTreeItem = new AttributeTreeItem(objAttTreePos.getId(), attType, inputViewNoValueFlag, position);

					attTreeItemList.add(attTreeItem);
				}

				if (objAttTreePosList.size() > 0) {
					attTreeItemList = AppObjectUtil.getIntSortedList(attTreeItemList, "getPosition", true);
				}

				String attTreeOtherName = getOtherAttributeTreeName(sess, objAttTree.getId(), sess.getLangId());

				if (attTreeOtherName == null) {
					attTreeOtherName = objAttTree.getName();
				}

				//属性ツリー生成
				return new AttributeTree(objAttTree.getId(),
						attTreeOtherName, objAttTree.getName(),
						AppObjectUtil.getIntAttr(sess, objAttTree, EIMConfig.get("ATTR_NAME_ATTRTREE_CLASS"), Integer.MIN_VALUE),
						attTreeItemList);
			}

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性値の条件に合致する属性値を取得します。
	 *
	 * <li>検索対象の値がnullの場合、nullを引数リストに設定して下さい。
	 * <li>該当がない場合は長さ0のリストを返却します。
	 * <li>「属性値なし」がある場合は返却リストにnullを追加します。
	 * <li>ログインユーザが読取権限のみの場合、「公開済」または「readのみ権限以外」または「ステータスがない、かつ、readのみ権限」の対象のみが返却される。
	 * <li>設定ファイルで指定の最大取得件数(GET_DATA_MAX_NUM)＋1件までを取得して返却します。
	 *
	 * @param sess セッション情報
	 * @param attTree ツリーを生成する属性ツリー情報
	 * @param upperAttributeValList 属性ツリーの上位の属性値を最上位から順に設定する配列
	 * @return 引数upperAttributeValueListの最下位の属性値の下に表示すべき属性値(String/Date/Integer)のリスト。属性値なしの場合は「null」を設定。
	 * @throws Exception
	 */
	public static List classifyAttributeValues(EIMSession sess, AttributeTree attTree,
							List upperAttributeValList)	 throws Exception {

		List retList = new ArrayList();
		Connection con = null;
		Connection ocon = null;
		PreparedStatement stmt = null;
		ResultSet rs = null;

		try {
			if (attTree != null) {
				if (attTree.getTreeItemList().size() < upperAttributeValList.size() + 1) {
					// 指定した属性ツリーより多くの属性値が指定されました。
					throw new EIMException(sess, "EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.TREE.MANY.VALUES");

				} else {

					StringBuffer joinSql = new StringBuffer();
					String selectSql = "";
					String groupByOrderBySql = "";
					Boolean isProc = false;              // 検索処理を実施するか否か
					List nullValueAttList = new ArrayList();
					int searchAttrTypeId = Integer.MIN_VALUE;

					for (int i = 0 ; upperAttributeValList.size() + 1 > i ; i++) {

						// 検索対象とすべき属性か否か
						boolean isTarget = (i == upperAttributeValList.size());

						// 検索条件となる属性値
						Object element = null;
						if (!isTarget) {
							element = (Object) upperAttributeValList.get(i);
						}

						// 属性ツリー所属属性
						AttributeTreeItem treeItem = (AttributeTreeItem)attTree.getTreeItemList().get(i);

						// 属性テーブル名
						String attTbl = "oat" + (i+1);

						// 引数属性タイプの型に対応したSQLのJOIN句を生成
						
						switch (treeItem.getType().getValueType().getId()) {

						// 文字列型の場合
						case EIMValueType.STRING:
							if (isTarget) {
								joinSql.append("left outer join eimobjstr " + attTbl + " on eo.id = " + attTbl + ".id"
										+ " and " + attTbl + ".type= " + treeItem.getType().getId() + " ");

							} else {
								if (element != null) {
									joinSql.append("inner join eimobjstr " + attTbl + " on eo.id = " + attTbl + ".id"
											+ " and " + attTbl + ".type = " + treeItem.getType().getId()
											+ " and " + attTbl + ".value = '" + (String)element + "' ");
								} else {
									// NULLが指定された場合
									nullValueAttList.add(treeItem.getType());
								}
							}
							break;

						// 数値型の場合
						case EIMValueType.INTEGER:
							if (isTarget) {
								joinSql.append("left outer join eimobjint " + attTbl + " on eo.id = " + attTbl + ".id"
										+ " and " + attTbl + ".type= " + treeItem.getType().getId() + " ");

							} else {
								if (element != null) {
									joinSql.append("inner join eimobjint " + attTbl + " on eo.id = " + attTbl + ".id"
											+ " and " + attTbl + ".type = " + treeItem.getType().getId()
											+ " and " + attTbl + ".value = " + (Long)element + " ");
								} else {
									// NULLが指定された場合
									nullValueAttList.add(treeItem.getType());
								}
							}
							break;
						//double型の場合
						case EIMValueType.DOUBLE:
							if (isTarget) {
								joinSql.append("left outer join eimobjreal " + attTbl + " on eo.id = " + attTbl + ".id"
										+ " and " + attTbl + ".type= " + treeItem.getType().getId() + " ");

							} else {
								if (element != null) {
									joinSql.append("inner join eimobjreal " + attTbl + " on eo.id = " + attTbl + ".id"
											+ " and " + attTbl + ".type = " + treeItem.getType().getId()
											+ " and " + attTbl + ".value = " + (Double)element + " ");
								} else {
									// NULLが指定された場合
									nullValueAttList.add(treeItem.getType());
								}
							}
							break;

						// 日付型の場合
						case EIMValueType.DATE:
							if (isTarget) {
								joinSql.append("left outer join eimobjdate " + attTbl + " on eo.id = " + attTbl + ".id"
										+ " and " + attTbl + ".type= " + treeItem.getType().getId() + " ");

							} else {
								if (element != null) {
									DateFormat dfm = new SimpleDateFormat("yyyy/MM/dd");
									String dateStr = dfm.format((Date)element);
									joinSql.append("inner join eimobjdate " + attTbl + " on eo.id = " + attTbl + ".id"
											+ " and " + attTbl + ".type = " + treeItem.getType().getId()
											+ " and to_char(trunc(" + attTbl + ".value), 'yyyy/MM/dd') = " + "'" + dateStr + "' ");
								} else {
									// NULLが指定された場合
									nullValueAttList.add(treeItem.getType());
								}
							}
							break;

						/**
						 * テキスト型を属性ツリービューにて表示しない仕様⇒コメントアウト。
						 * */

						/**
						 * 飯山追加。
						 * CLOB型へ変更の為、＝演算子が使用不可。
						 * DBMS_LOB.compare()関数を利用して対応。
						 **/

						/*// テキスト型の場合
						case EIMValueType.TEXT:
							if (isTarget) {
								joinSql.append("left outer join eimobjtext " + attTbl + " on eo.id = " + attTbl + ".id"
										+ " and " + attTbl + ".type= " + treeItem.getType().getId() + " ");

								// GROUP BY句、ORDER BY句の生成
								groupByOrderBySql =  "group by to_char("+attTbl+".value)";

								// 「属性値なしは非表示」の場合のみ
								if (treeItem.isViewNoValues() == false) {
									groupByOrderBySql += " having to_char("+attTbl+".value) is not null";
								}
								groupByOrderBySql += " order by to_char("+attTbl+".value)";


								// SELECT句の生成
								selectSql = "to_char("+attTbl+".value) as value ";

							} else {

								if (element != null) {
									joinSql.append("inner join eimobjtext " + attTbl + " on eo.id = " + attTbl + ".id"
											+ " and " + attTbl + ".type = " + treeItem.getType().getId()
											+" and DBMS_LOB.compare("+attTbl+".value,"+"'"+(String)element+"'"+")"+"=0");
											//+ " and " + attTbl + ".value = '" + (String)element + "' ");

								} else {
									// NULLが指定された場合
									nullValueAttList.add(treeItem.getType());
								}
							}
							break;
*/
							default:	//それ以外
								break;
						}

						if (isTarget) {
							
							if(treeItem.getType().getValueType().getId() != EIMValueType.TEXT){
								// SELECT句の生成
								selectSql = attTbl + ".value as value ";

								// GROUP BY句、ORDER BY句の生成
								//groupByOrderBySql =  "group by " + attTbl + ".value";

								// 「属性値なしは非表示」の場合のみ
								if (treeItem.isViewNoValues() == false) {
									//groupByOrderBySql += " having " + attTbl + ".value is not null";
									groupByOrderBySql += " and " + attTbl + ".value is not null";

								}
								groupByOrderBySql += " order by " + attTbl +  ".value";

								// 検索対象の属性タイプID
								searchAttrTypeId = treeItem.getType().getValueType().getId();
								
								// 検索対象の属性がTEXT属性ではない場合、検索処理を実施する(フラグを立てる)
								isProc = true;
							}

						}
					}

					// 分類対象別のオブジェクトタイプの取得
					List objTypeList = getObjTypeListForCategory(sess, attTree.getClassifyTarget());

					// Where句の生成 (オブジェクトタイプ条件)
					StringBuffer whereSqlForObjId = new StringBuffer();
					whereSqlForObjId.append("eo.type in (");
					for (int i = 0 ; i < objTypeList.size() ; i++) {
						if (i != 0) {
							whereSqlForObjId.append(", ");
						}
						EIMObjectType objType = (EIMObjectType)objTypeList.get(i);
						whereSqlForObjId.append(objType.getId());
					}
					whereSqlForObjId.append(") ");

					// Where句の生成 (null指定)
					StringBuffer whereSqlForNotExist = new StringBuffer();
					if (nullValueAttList.size() > 0) {

						whereSqlForNotExist.append("and eo.id in (select eo2.id from EIMOBJ eo2 where ");

						// null指定の属性値毎に「not exists」のwhere句を生成
						for (int i = 0 ; i < nullValueAttList.size() ; i++) {
							EIMAttributeType attType = (EIMAttributeType)nullValueAttList.get(i);
							if (i != 0) {
								whereSqlForNotExist.append("and ");
							}
							String attTableName = null;
							switch (attType.getValueType().getId()) {
							case EIMValueType.STRING:	// 文字列型
								attTableName = "EIMOBJSTR";
								break;
							case EIMValueType.INTEGER:	// 数値型
								attTableName = "EIMOBJINT";
								break;
							case EIMValueType.DOUBLE:	// double数値
								attTableName = "EIMOBJREAL";
								break;
							case EIMValueType.DATE:		// 日付型
								attTableName = "EIMOBJDATE";
								break;

								/**
								 * テキスト型を属性ツリービューにて表示しない仕様⇒コメントアウト。
								 * */
/*
							case EIMValueType.TEXT:		// テキスト型
								attTableName = "EIMOBJTEXT";
								break;
*/
							default:	//それ以外
								break;
							}
							whereSqlForNotExist.append("not exists (select * from " + attTableName + " eoa " +
									"where eoa.id = eo2.id and eoa.type = " + attType.getId() + ") ");
						}
						whereSqlForNotExist.append(") ");
					}

					// 読取権限のみの場合
					StringBuffer whereSqlForReadOnly = new StringBuffer();
					List readOnlySecurities = SecurityUtils.getReadOnlySecurityList(sess);
					if (readOnlySecurities.size() > 0) {

						// 公開済
						whereSqlForReadOnly.append(
								"and ( " +
									"EO.status in ( " +
										"select R0.sid as key from EIMST R0 " +
										"inner join EIMSTTYPE R1 on R0.type = R1.id " +
										"where R1.kind = " + AppConstant.STATUS_TYPE_KIND_ID_PUBLIC + ") ");

						// readのみ権限以外
						whereSqlForReadOnly.append("or EO.security not in ( ");
						for (int i = 0 ; readOnlySecurities.size() > i ; i++) {
							if (i != 0) {
								whereSqlForReadOnly.append(", ");
							}
							whereSqlForReadOnly.append(((EIMSecurity)readOnlySecurities.get(i)).getId());
						}
						whereSqlForReadOnly.append(") ");

						// ステータスがない、かつ、readのみ権限
						whereSqlForReadOnly.append(
								"or (EO.id in (select id from EIMOBJ " +
								"where status = " + AppConstant.STATUS_TYPE_KIND_ID_NONE + " " +
								"and latest = " + AppConstant.LATEST_HISTORY_FLAG_ON + " " +
								"and EO.security in (");
						for (int i = 0 ; readOnlySecurities.size() > i ; i++) {
							if (i != 0) {
								whereSqlForReadOnly.append(", ");
							}
							whereSqlForReadOnly.append(((EIMSecurity)readOnlySecurities.get(i)).getId());
						}
						whereSqlForReadOnly.append(")))) ");
					}
					//完全対SQLの生成
					String sql =
						"select eo.id as id, " + selectSql +
						" from ( eimobj eo " + joinSql.toString() +
						") " +
						"where " +
						"(" +
							"(" +
							"EO.security = 0 or " +
							"EO.security in " +
							"(" +
								"select " +
									"TMP.sid " +
								"from " +
									"(" +
										"select " +
											"eid, " +
											"sid, " +
											"entry ," +
											"role ," +
											"permit ," +
											"rank() over (partition by sid, entry order by priority) as ranks " +
										"from " +
										"	EIMACR ACR " +
												"inner join " +
											"EIMACU ACU " +
											"on " +
											"	ACR.id = ACU.eid " +
											"where " +
											"ACU.entry = " + sess.getUser().getId() + " "+
										"and ACR.permit != 2 " +
									") TMP" +
								" where " +
									"TMP.ranks = 1 " +
								"and TMP.permit = 1 " +
								"and TMP.role = 12 "+
//								"group by TMP.sid " +
							") " +
						") and " +
						whereSqlForObjId.toString() + whereSqlForNotExist.toString() + whereSqlForReadOnly.toString() +
					" ) "+ groupByOrderBySql;
										
					// <最大取得件数> + 1件までを取得
					int maxnum = Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM")) + 1;
					sql = "select * from (" + sql + ") TMP2 " +
							DatabasePlugInLoader.getPlugIn().getQueryStringLimitRowNumber();
					
					// 検索処理実施フラグがtrueの場合
					if(isProc){
						// SQL実行
						con = sess.getDBConnection();
						ocon = DBUtils.getNativeConnection(con);
						stmt = ocon.prepareStatement(sql, ResultSet.TYPE_SCROLL_INSENSITIVE, ResultSet.CONCUR_UPDATABLE);
						stmt.setLong(1, maxnum);
						rs = stmt.executeQuery();
						
						// 検索条件追加プラグインに対応させるため
						// 取得できたオブジェクトのIDを振るいにかける
						List<EIMRelation> dspObjList = AppSearchUtils.searchRelationByConditionMaker(sess, 
								EIMDocSearchType.DISPLAY_ATTRTREE_LIST, EIMAccessRole.READ, 
								new EIMSearchLimitCountCondition(EIMSearchLimitCountCondition.NOT_SPECIFIED, false), rs);
						Map<Long, EIMObject> dspObjMap = new HashMap<Long, EIMObject>();
						for (EIMRelation relation : dspObjList) {
							EIMObject object = relation.getChild();
							dspObjMap.put((long)object.getId(), object);
						}

						// 検索結果の取得
						rs.beforeFirst();
						while (rs.next()) {
							
							if (!dspObjMap.containsKey((long)rs.getLong("id"))) {
								continue;
							}
							
							if (checkOverlapValue(sess, rs, searchAttrTypeId, retList, dspObjMap.get((long)rs.getLong("id")))) {
								continue;
							}

							switch (searchAttrTypeId) {

							// 文字列型の場合
							case EIMValueType.STRING:
								String strValue = rs.getString("value");
								if (strValue == null) {
									retList.add(null);
								} else {
									retList.add(strValue);
								}
								break;

							// 数値型の場合
							case EIMValueType.INTEGER:
								long intValue = rs.getLong("value");
								if (rs.wasNull()) {
									retList.add(null);
								} else {
									retList.add(new Long(intValue));
								}
								break;
							
							// double数値型の場合
							case EIMValueType.DOUBLE:
								// スクロール可能な結果セットを使用している、getDouble()では
								// システムエラーが発生するのでgetObject()を使用
								Object objectValue = rs.getObject("value");
								if (rs.wasNull()) {
									retList.add(null);
								} else {
									double doubleValue = Double.parseDouble(objectValue.toString());
									retList.add(new Double(doubleValue));
								}
								break;

							// 日付型の場合
							case EIMValueType.DATE:
								Timestamp timeStampValue = rs.getTimestamp("value");
								if (timeStampValue == null) {
									retList.add(null);
								} else {
									retList.add(new Date(timeStampValue.getTime()));
								}
								break;

								/**
								 * テキスト型を属性ツリービューにて表示しない仕様⇒コメントアウト。
								 * */
	/*
							// テキスト型の場合
							case EIMValueType.TEXT:
								String textValue = rs.getString("value");
								if (textValue == null) {
									retList.add(null);
								} else {
									retList.add(textValue);
								}
								break;
	*/
							default:	//それ以外
								break;
							}
						}
					}

				}
			}
			return retList;

		} catch (EIMException eime) {
			throw eime;

		} catch (Exception e) {
			throw e;

		} finally {
			if (rs != null) {
				rs.close();
			}
			if (stmt != null) {
				stmt.close();
			}
		}
	}
	
	/**
	 * 表示するオブジェクトの重複をチェックする
	 * 重複していない場合はリレーション検索を行い、親が表示可能か確認する
	 * @param sess
	 * @param rs
	 * @param searchAttrTypeId
	 * @param retList
	 * @param eimObj
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private static boolean checkOverlapValue(EIMSession sess, ResultSet rs,
			int searchAttrTypeId, List retList, EIMObject eimObj) throws Exception {
		
		boolean result = false;
		
		switch (searchAttrTypeId) {
		case EIMValueType.STRING:
			String strValue = rs.getString("value");
			result = retList.contains(strValue);
			break;
		case EIMValueType.INTEGER:
			long intValue = rs.getLong("value");
			if (rs.wasNull()) {
				result = retList.contains(null);
			} else {
				result = retList.contains(intValue);
			}
			break;
		case EIMValueType.DOUBLE:
			// スクロール可能な結果セットを使用している、getDouble()では
			// システムエラーが発生するのでgetObject()を使用
			Object objectValue = rs.getObject("value");
			double doubleValue = Double.NaN;
			if (objectValue != null) {
				doubleValue = Double.parseDouble(objectValue.toString());
			}
			for (Object obj : retList) {
				if (obj == null) {
					if (rs.wasNull()) {
						result = true;
						break;
					} else {
						continue;
					}
				}
				double value = ((Double)obj).doubleValue();
				if (doubleValue == value) {
					result = true;
					break;
				}
			}
			break;
		case EIMValueType.DATE:
			Timestamp timeStampValue = rs.getTimestamp("value");
			for (Object obj : retList) {
				if (obj == null) {
					if (timeStampValue == null) {
						result = true;
						break;
					} else {
						continue;
					}
				}
				long value = ((Date)obj).getTime();
				if (timeStampValue == null) {
					continue;
				}
				if (timeStampValue.getTime() == value) {
					result = true;
					break;
				}
			}
			break;
		default:
			result = false;
			break;
		}
		
		return result;
	}

	/**
	 * 属性値の条件に合致するEIMObjectを取得します。
	 *
	 * <li>検索対象の値がnullの場合、nullを引数リストに設定して下さい。
	 * <li>該当がない場合は長さ0のリストを返却します。
	 * <li>設定ファイルで指定の最大取得件数(GET_DATA_MAX_NUM)＋1件までを取得して返却します。
	 *
	 * @param sess セッション情報
	 * @param attTree ツリーを生成する属性ツリー情報
	 * @param upperAttributeValList 属性ツリーの上位の属性値を最上位から順に設定する配列
	 * @return 引数upperAttributeValueListの最下位の属性値の下に表示すべきフォルダを示したEIMObjectのリスト
	 * @throws Exception
	 */
	public static List getClassifiedObjects(EIMSession sess, AttributeTree attTree,
			List upperAttributeValList)	throws Exception {

		List retList = new ArrayList();

		try {
			if (attTree != null) {
				if (attTree.getTreeItemList().size() != upperAttributeValList.size()) {
					// 指定した属性ツリーとは違う数の属性値が指定されました。
					throw new EIMException(sess, "EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.TREE.INVALID.VALUE.NUM");

				} else {

					// 検索条件項目・返却項目指定オブジェクトを生成
					// <最大取得件数> + 1件までを取得
					Object[] userData = {attTree, upperAttributeValList};
					int maxnum = Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM")) + 1;
					retList = AppSearchUtils.searchObjectsByConditionMaker(sess, 
							EIMDocSearchType.DISPLAY_ATTRTREE_OBJLIST, EIMAccessRole.READ, 
							new EIMSearchLimitCountCondition(maxnum, false), userData);

					if (retList.size() > 0) {

						// フォルダの場合、下位フォルダを除去
						if (attTree.getClassifyTarget() == AppConstant.CLASSIFY_TARGET_FOLDER) {

							// 検索結果格納Set [key]オブジェクトID
							HashSet objSet = new HashSet();
							// 親フォルダ所持フォルダ削除結果格納Set [key]オブジェクトID
							HashSet resultObjSet = new HashSet();
							for (Iterator iter = retList.iterator(); iter.hasNext();) {
								EIMObject obj = (EIMObject) iter.next();
								objSet.add(new Long(obj.getId()));
							}

							for (Iterator iter = retList.iterator(); iter.hasNext();) {
								EIMObject obj = (EIMObject) iter.next();

								// 親オブジェクト配列を取得
								EIMObject[] parentObjs = AppObjectUtil.getParentEIMObject(sess, obj, EIMConfig.get("RELATION_TYPE_NAME_DOCUMENT"));

								if (parentObjs.length > 0) {
									// 親オブジェクトが検索結果の中に存在する場合、自分自身を除去
									for (int i = 0 ; i < parentObjs.length; i++) {
										if (!objSet.contains(new Long(parentObjs[i].getId()))) {
											resultObjSet.add(new Long(obj.getId()));
											break;
										}
									}
								} else {
									resultObjSet.add(new Long(obj.getId()));
								}
							}

							List tmpList = new ArrayList();
							for (Iterator iter = retList.iterator(); iter.hasNext();) {
								EIMObject obj = (EIMObject) iter.next();
								if (resultObjSet.contains(new Long(obj.getId()))) {
									tmpList.add(obj);
								}
							}
							retList = tmpList;
						}
					}
				}
			}
			return retList;

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}


	

	/**
	 * 分類対象別にオブジェクトタイプの一覧を取得します。
	 *
	 * @param sess セッション情報
	 * @param categoryId 分類対象のID
	 * @return オブジェクトタイプの一覧
	 * @throws Exception
	 */
	public static List getObjTypeListForCategory(EIMSession sess, long categoryId) throws Exception {

		List objTypeList = new ArrayList();
		if (categoryId == AppConstant.CLASSIFY_TARGET_FOLDER) {
			// フォルダの場合
			EIMObjectType folderType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_FOLDER"));
			ObjectUtils.getChildObjectTypeListRecurrently(sess, folderType, objTypeList);
			objTypeList.add(folderType);
		} else {
			// フォルダ以外(＝ドキュメント)の場合
			EIMObjectType docType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_DOCUMENT"));
			ObjectUtils.getChildObjectTypeListRecurrently(sess, docType, objTypeList);
			objTypeList.add(docType);
		}
		return objTypeList;
	}

	/**
	 * 属性ツリーIDに対応する属性ツリー他言語オブジェクトのリストを取得します。
	 * 属性ツリー他言語オブジェクトが存在しなかった場合は空のリストを返却します。
	 * @param sess セッション
	 * @param attTreeId 属性ツリーID
	 * @return List 属性ツリー他言語オブジェクトのリスト
	 * @throws EIMException
	 * 				ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	private static List getOtherAttributeTreeByAttTreeId(EIMSession sess,
			long attTreeId) throws Exception
	{
		try {
			//「属性ツリー他言語」オブジェクトタイプを取得
			EIMObjectType attrTreeOtherObjType = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEOTHER"));

			//検索条件項目・返却項目指定オブジェクトを生成
			EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();

			//検索条件グループ作成
			EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			//条件１:オブジェクトタイプが「属性ツリー他言語」を検索
			EIMAttributeType fieldOfObjType = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE;
			EIMSearchConditionCompare cond1 = new EIMSearchConditionCompare(
					EIMSearchOperatorEnum.AND
					, fieldOfObjType				//TYPE
					, EIMSearchOperatorEnum.EQ		// ==
					, attrTreeOtherObjType.getId() 	//'属性ツリー他言語'
				);
			//条件１を条件グループに加える
			conds.addCondition(cond1);

			//属性ツリーIDを文字列に変換(オブジェクト名称)
			String objName = Long.toString(attTreeId) + "_*";

			//条件２: and オブジェクト名が<入力引数>を検索
			EIMAttributeType fieldOfObjName = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME;
			EIMSearchConditionLike cond2 = new EIMSearchConditionLike(
				EIMSearchOperatorEnum.AND	//前方演算子AND
				, fieldOfObjName			//オブジェクト名
				, EIMSearchOperatorEnum.LIKE	// ==
				, objName				//入力引数「オブジェクトID」
			);
			//条件２を条件グループに加える
			conds.addCondition(cond2);

			//検索条件項目・返却項目指定インスタンスに条件グループを設定
			selectTarget.setCondition(conds);

			List attrTreeOtherObjList = new ArrayList();

			//検索実行
			selectTarget.setRole(EIMAccessRole.READ);
			attrTreeOtherObjList = SearchUtils.searchObjects(sess, selectTarget, null);

			return attrTreeOtherObjList;

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 属性ツリーIDに対応する属性ツリー所属属性オブジェクトのリストを取得します。
	 * 属性ツリー所属属性オブジェクトが存在しなかった場合は空のリストを返却します。
	 * @param sess セッション
	 * @param attTreeId 属性ツリーID
	 * @return List 属性ツリー所属属性オブジェクトのリスト
	 * @throws EIMException
	 * 				ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	private static List getAttrTreePosByAttTreeId(EIMSession sess,
			long attTreeId) throws Exception
	{
		try {
			//「属性ツリー所属属性」オブジェクトタイプを取得
			EIMObjectType objTypeAttrTreePos = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRTREEPOS"));

			//検索条件項目・返却項目指定オブジェクトを生成
			EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();

			//検索条件グループ作成
			EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			//条件１:オブジェクトタイプが「属性ツリー所属属性」を検索
			EIMAttributeType fieldOfObjType = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE;
			EIMSearchConditionCompare cond1 = new EIMSearchConditionCompare(
					EIMSearchOperatorEnum.AND
					, fieldOfObjType				//TYPE
					, EIMSearchOperatorEnum.EQ		// ==
					, objTypeAttrTreePos.getId() 	//'属性ツリー所属属性'
				);
			//条件１を条件グループに加える
			conds.addCondition(cond1);

			//属性ツリーIDを文字列に変換(オブジェクト名称)
			String objName = Long.toString(attTreeId) + "_*";

			//条件２: and オブジェクト名が<入力引数>を検索
			EIMAttributeType fieldOfObjName = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME;
			EIMSearchConditionLike cond2 = new EIMSearchConditionLike(
				EIMSearchOperatorEnum.AND	//前方演算子AND
				, fieldOfObjName			//オブジェクト名
				, EIMSearchOperatorEnum.LIKE	// ==
				, objName				//入力引数「オブジェクトID」
			);
			//条件２を条件グループに加える
			conds.addCondition(cond2);

			//検索条件項目・返却項目指定インスタンスに条件グループを設定
			selectTarget.setCondition(conds);

			List attrTreePosList = new ArrayList();

			//検索実行
			selectTarget.setRole(EIMAccessRole.READ);
			attrTreePosList = SearchUtils.searchObjects(sess, selectTarget, null);

			return attrTreePosList;

		} catch (EIMException eime) {
			throw eime;
		} catch (Exception e) {
			throw e;
		}
	}

	/**
	 * 操作履歴テーブルに属性ツリークラスの操作履歴を追加します。<br>
	 *
	 * OperationHistoryUtilsは引数rcObjAにAttributeTreeクラスを指定できません。
	 * 本メソッドを使用して属性ツリークラスの操作履歴を記録してください。
	 *
	 * @param sess EIMSessionインスタンス
	 * @param appType アプリケーション種別
	 * @param opType 操作種別
	 * @param rcInfoA 操作対象情報 A
	 * @param rcTypeA 操作対象種別 A
	 * @param rcObjA 操作対象 A：AttributeTreeクラスのインスタンス
	 * @param rcInfoB 操作対象情報 B
	 * @param rcTypeB 操作対象種別 B
	 * @param rcObjB 操作対象 B
	 * @param detail 操作詳細
	 * @throws Exception
	 */
	public static void createOperationHistory(EIMSession sess, String appType,
			String opType, String rcInfoA, String rcTypeA,
			AttributeTree rcObjA, String rcInfoB, String rcTypeB,
			Object rcObjB, String detail) throws Exception {
		// 操作履歴用の言語
		String langId = EIMConfig.get("OPERATION_HISTORY_LANG");
		if (StringUtils.isBlank(langId)) {
			// 操作履歴用の言語が取得できませんでした。
			throw new EIMException(sess, EIMConstant.ERR_OPEHIST_LANG_NOTFOUND);
		}

		// 操作履歴用の言語での名前を取得
		String objName = getOtherAttributeTreeName(sess, rcObjA.getId(), langId);
		// 取れなければデフォルト名を使用
		if (objName == null)
			objName = rcObjA.getDefName();

		// 操作履歴用のダミーのEIMOBJを生成。
		// eimobj.idにはattrTree.id、eimobj.nameにはattrTreeの他言語名をセットする。
		EIMObject histObj = new EIMObject(rcObjA.getId(), null, objName, 0,
				true, null, null, null, null, null, null, false, false, null);

		// 操作履歴を記録
		OperationHistoryUtils.create(sess, appType, opType, rcInfoA, rcTypeA,
				histObj, rcInfoB, rcTypeB, rcObjB, detail);
	}

	/**
	 * http requestを解析し、結果を返します
	 *
	 * @param request Httpリクエスト
	 * @return 解析結果HttpParamParsedインスタンス
	 */
	public static HttpParamParsed parseHttpParam(HttpServletRequest request) {
		List paramIds = new ArrayList();

		String attrTreeId = request.getParameter("attrTreeId");
		paramIds.add("attrTreeId=" + attrTreeId);

		String attrTreeSettings = request.getParameter("attrTreeSettings");
		paramIds.add("attrTreeSettings=" + attrTreeSettings);

		List attrTreeValues = new ArrayList();
		for (int i = 1;; i++) {
			String paramName = "attrTreeValue" + i;
			String attrTreeValue = EIMUtils.getParameter(request, paramName);
			if (attrTreeValue == null)
				break;
			paramIds.add(paramName + "=" + attrTreeValue);
			attrTreeValues.add(attrTreeValue);
		}
		return new HttpParamParsed(attrTreeId, attrTreeSettings, attrTreeValues, paramIds.toArray());
	}

	/**
	 * FLEXからのHTTPパラメータ保持クラス
	 */
	public static class HttpParamParsed {
		/** 属性ツリーID */
		private String _attrTreeId;

		/** 属性ツリー設定 */
		private String _attrTreeSettings;

		/** 属性ツリー値リスト */
		private List _attrTreeValues;

		/** ログ用パラメータ集 */
		private Object[] _paramIds;

		/**
		 * コンストラクタ
		 * @param attrTreeId 属性ツリーID
		 * @param attrTreeSettings 属性ツリー設定
		 * @param attrTreeValues 属性ツリー値リスト
		 * @param paramIds ログ用パラメータ集
		 */
		HttpParamParsed(String attrTreeId, String attrTreeSettings, List attrTreeValues,
				Object[] paramIds) {
			_attrTreeId = attrTreeId;
			_attrTreeSettings = attrTreeSettings;
			_attrTreeValues = attrTreeValues;
			_paramIds = paramIds;
		}

		/**
		 * 属性ツリーIDを返します
		 * @return 属性ツリーID
		 */
		public String getAttrTreeId() {
			return _attrTreeId;
		}

		/**
		 * 属性ツリー設定を返します
		 * @return 属性ツリー設定
		 */
		public String getAttrTreeSettings() {
			return _attrTreeSettings;
		}

		/**
		 * 属性ツリー値リストを返します
		 * @return 属性ツリー値リスト
		 */
		public List getAttrTreeValues() {
			return _attrTreeValues;
		}

		/**
		 * ログ用パラメータ集を返します
		 * @return ログ用パラメータ集
		 */
		public Object[] getParamIds() {
			return _paramIds;
		}
	}

	/**
	 * 引数のリスト(List<Sring>)内の文字列(Httpリクエスト値)を、属性ツリー項目の型に応じてDate,Integer型に型変換して更新します
	 *
	 * @param attrTree 属性ツリー
	 * @param attrTreeValues 変換するString値(Httpリクエスト値)のリスト
	 */
	public static void convertAttrTreeValueToDataType(AttributeTree attrTree, List attrTreeValues) {
		List attrTreeItems = attrTree.getTreeItemList();
		for (int i = 0; i < attrTreeValues.size(); i++) {
			if (attrTreeItems.size() <= i) {// 属性ツリー項目数に対し、画面から飛んできたパラメータ数がオーバーしていないか？
				if (attrTree.isClassifyTargetDocument()) {
					// ドキュメント分類なのに、オーバーしている。属性ツリー定義が変わっているので、展開はやめる
					attrTreeValues.clear();
					break;
				} else {
					// フォルダ分類。以降はフォルダのID。Longにする。
					attrTreeValues.set(i, Long.valueOf((String) attrTreeValues.get(i)));
					continue;
				}
			}
			String value = (String) attrTreeValues.get(i);
			if (value.length() == 0) {
				// 属性値なしを意味するブランク値をnullに変換する
				attrTreeValues.set(i, null);
				continue;
			}
			AttributeTreeItem item = (AttributeTreeItem) attrTreeItems.get(i);
			switch (item.getType().getValueType().getId()) {
			case EIMValueType.DATE:
				// 日付値を意味するlong値をDateに変換する
				attrTreeValues.set(i, new Date(Long.parseLong(value)));
				break;
			case EIMValueType.INTEGER:
				// 数値を意味するint値をIntegerに変換する
				attrTreeValues.set(i, Long.valueOf(value));
				break;
			case EIMValueType.DOUBLE:
				// 数値を意味するdouble値をIntegerに変換する
				attrTreeValues.set(i, Double.valueOf(value));
				break;
			}
		}
	}

	/**
	 * 属性ツリーの設定情報を文字列にして返します
	 *
	 * @param sess セッション
	 * @param attrTree 対象属性ツリー定義
	 * @return 属性ツリーの設定情報を文字列にしたもの
	 * @throws Exception
	 */
	public static String getAttrTreeSettingsStr(EIMSession sess, AttributeTree attrTree)
			throws Exception {
		StringBuffer sb = new StringBuffer(String.valueOf(attrTree.getClassifyTarget()));
		for (int i = 0; i < attrTree.getTreeItemList().size(); i++) {
			AttributeTreeItem item = (AttributeTreeItem) attrTree.getTreeItemList().get(i);
			if (item.getType() == null) {
				// このメソッドではエラーにしない。checkAndGetAttrTree()でエラーとなる
				sb.append(",");
				sb.append("[unknown(" + i + ")]");
			} else {
				sb.append(",");
				sb.append(item.getType().getId());
			}
			sb.append(",");
			sb.append(item.isViewNoValues());
		}
		return sb.toString();
	}

	/**
	 * Httpパラメータを解析して、指定された属性ツリーを返します。
	 *
	 * @param sess セッション
	 * @param param Httpパラメータ
	 * @return 属性ツリー。Httpパラメータで属性ツリーIDが無指定の場合はnull
	 * @throws EIMException 指定属性ツリーが見つからない、属性が無い、属性ツリー定義が変更されている場合
	 * @throws Exception
	 */
	public static AttributeTree checkAndGetAttrTree(EIMSession sess, HttpParamParsed param)
			throws Exception {
		if (StringUtils.isBlank(param.getAttrTreeId()))
			return null;
		AttributeTree attrTree = AttributeTreeUtil.getAttributeTreeById(sess,
			Long.parseLong(param.getAttrTreeId()));
		// 選択属性ツリーが存在しない場合、もしくは属性ツリー項目が無く使用不能な場合
		if (attrTree == null || attrTree.getTreeItemList().size() == 0)
			throw new EIMException(sess, "EIM.ERROR.LOGIC.NOATTRTREE");

		// 選択属性値の属性タイプが存在しない場合
		for (Iterator i = attrTree.getTreeItemList().iterator(); i.hasNext();) {
			if (((AttributeTreeItem) i.next()).getType() == null)
				throw new EIMException(sess, "EIM.ERROR.LOGIC.NOATTRTYPE");
		}

		// 選択属性ツリーの定義が変更されている場合
		if (!StringUtils.isBlank(param.getAttrTreeSettings())
				&& !getAttrTreeSettingsStr(sess, attrTree).equals(param.getAttrTreeSettings()))
			throw new EIMException(sess, "EIM.ERROR.LOGIC.ATTRTREE.REG.CHANGED");

		return attrTree;
	}

	/**
	 * 引数例外が、checkAndGetAttrTreeメソッドが返したチェックエラーかどうかを返します
	 *
	 * @param e 対象例外
	 * @return checkAndGetAttrTreeメソッドが返したチェックエラーならtrue
	 */
	public static boolean isErrorOfCheckAndGetAttrTree(EIMException e) {
		String messageKey = e.getMessageKey();
		return messageKey.equals("EIM.ERROR.LOGIC.NOATTRTREE")
				|| messageKey.equals("EIM.ERROR.LOGIC.NOATTRTYPE")
				|| messageKey.equals("EIM.ERROR.LOGIC.ATTRTREE.REG.CHANGED");
	}

	/**
	 * 属性ツリーの内、属性ツリー項目を持つものを全て取得し、AttributeTreeのリストに設定して返却します。<br>
	 * 属性ツリーが存在しない場合、空のリストを返却する。
	 *
	 * @param sess セッション
	 * @return 属性ツリーのリスト
	 * @throws EIMException ストアドプロシージャで例外が発生した場合
	 * @throws Exception その他の例外が発生した場合
	 */
	public static List getAttributeTreeListOnlyHavingItem(EIMSession sess) throws Exception {
		List resultList = getAttributeTreeList(sess);
		for (Iterator i = resultList.iterator(); i.hasNext();) {
			if (((AttributeTree) i.next()).getTreeItemList().size() == 0)
				i.remove();
		}
		return resultList;
	}
}