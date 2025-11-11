package eim.command.common.util;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchConditionLike;
import eim.bo.EIMSearchConditionRange;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.bo.EIMValueType;
import eim.command.common.EIMCommandSearchTarget;
import eim.command.common.ResultNoWFStsObjList;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.SearchUtils;
import eim.util.TypeConvertUtils;
import eim.util.internal.search.SearchCondition;

/**
 * EIMCommandの検索実行クラス
 *
 *
 */
public class EIMCommandSearchUtils {

	/**
	 * findコマンドのデフォルト返却項目
	 */
	private static EIMAttributeType[] FIND_COMMAND_DEFAULT_OBJ_TYPE_ATTRTYPE = {
									PsedoAttributeTypeEnum.TYPE,		// オブジェクトタイプ
									PsedoAttributeTypeEnum.NAME,		// オブジェクト名称
									PsedoAttributeTypeEnum.CUSER,		// オブジェクト作成者
									PsedoAttributeTypeEnum.CDATE,		// オブジェクト作成日
									PsedoAttributeTypeEnum.MUSER,		// オブジェクト更新者
									PsedoAttributeTypeEnum.MDATE,		// オブジェクト更新日
									PsedoAttributeTypeEnum.STATUS,		// オブジェクトステータス
									PsedoAttributeTypeEnum.SECURITY		// オブジェクトのセキュリティ
	};

	/**
	 * 検索の実行
	 * @param sess EIMセッション
	 * @param target 検索条件データ
	 * @param includeAttr 検索結果に属性値を含むか(true:含む、false:含まない)
	 * @param roleId 検索対象権限ロールID
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static List<EIMSearchResultList> seachObject(EIMSession sess, EIMCommandSearchTarget target, Boolean includeAttr, long roleId) throws Exception
	{
		// 検索条件項目設定オブジェクト
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();

		// 検索条件グループ
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		// 検索条件：オブジェクトタイプ
		conds.addCondition(setSearchConditionObjType(target));

		// 検索条件：オブジェクト名(複数指定の場合は、OR検索)
		conds.addCondition(setSearchConditionObjName(target));

		// 検索条件：パス(複数指定の場合は、OR検索)
		// 属性「パス」
		EIMAttributeType attrTypePath = AttributeUtils.getAttributeTypeByName(sess, EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		conds.addCondition(setSearchConditionPath(target, attrTypePath));

		// 検索条件：属性値(同じ属性のペアを複数回指定した場合は、OR検索)
		conds.addCondition(setSearchConditionAttr(sess, target));

		// 返却項目の設定
		if(!includeAttr)
		{
			setResultAttrs(sess, selectTarget);
		}

		// 検索条件項目にセット
		selectTarget.setCondition(conds);

		// 検索対象権限の設定
		selectTarget.setRole(roleId);

		// 検索の実行
		// (検索結果の上限は、configにて定義。上限を超えた場合、Exceptionを投げる)
		return SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(Integer.parseInt(EIMConfig.get("GET_DATA_MAX_NUM")),true));
	}


	/**
	 * オブジェクトタイプについて、検索条件を追加する(OR検索、完全一致)
	 * @param target
	 * @return
	 */
	private static EIMSearchConditionGroup setSearchConditionObjType(EIMCommandSearchTarget target)
	{
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		for(Long objTypeId : target.getObjTypeIdList())
		{
			EIMSearchConditionCompare cond = new EIMSearchConditionCompare(
													EIMSearchOperatorEnum.OR, 								// 前方演算子「OR」
													EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, 	// 疑似属性「オブジェクトタイプID」
													EIMSearchOperatorEnum.EQ, 								// 比較演算子「＝」
													objTypeId);												// 値「オブジェクトタイプID」

			// 検索条件に追加する
			conds.addCondition(cond);
		}

		return conds;
	}


	/**
	 * オブジェクト名称について、検索条件を追加する(OR検索、完全一致、ワイルドカード使用可)
	 * @param target
	 * @return
	 */
	private static EIMSearchConditionGroup setSearchConditionObjName(EIMCommandSearchTarget target)
	{
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		for(String objName : target.getObjNameList())
		{
			EIMSearchConditionLike cond = new EIMSearchConditionLike(
													EIMSearchOperatorEnum.OR,								// 前方演算子「OR」
													EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME,	// 疑似属性「オブジェクト名称」
													EIMSearchOperatorEnum.LIKE,								// 比較演算子「＝」
													objName);												// 値「オブジェクト名称」

			// 検索条件に追加する
			conds.addCondition(cond);
		}

		return conds;
	}


	/**
	 * パス属性について、検索条件を追加する(OR検索、完全一致、ワイルドカード使用可)
	 * @param target
	 * @param attrTypePath
	 * @return
	 */
	private static EIMSearchConditionGroup setSearchConditionPath(EIMCommandSearchTarget target, EIMAttributeType attrTypePath) throws Exception
	{
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		for(String path : target.getPathList())
		{
			EIMSearchConditionLike cond = new EIMSearchConditionLike(
													EIMSearchOperatorEnum.OR,	// 前方演算子「OR」
													attrTypePath,				// 属性「パス」
													EIMSearchOperatorEnum.LIKE,	// 比較演算子「like」
													path);						// 値「パス」

			// 検索条件に追加する
			conds.addCondition(cond);
		}

		return conds;
	}


	/**
	 * 属性名と属性値について、検索条件を追加する(同様の属性名である場合、その値はOR検索。異なる属性名の場合、AND検索。完全一致。)
	 * @param sess
	 * @param target
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private static EIMSearchConditionGroup setSearchConditionAttr(EIMSession sess, EIMCommandSearchTarget target) throws Exception
	{
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		Iterator<Long> it = target.getAttrMap().keySet().iterator();
		while(it.hasNext())
		{
			// 属性タイプIDから属性タイプを取得
			Long tmpAttrTypeId = it.next();
			EIMAttributeType tmpAttrType = AttributeUtils.getAttributeTypeById(sess, tmpAttrTypeId);

			// 属性値のリスト
			List<String> attrValueList = (List<String>)target.getAttrMap().get(tmpAttrTypeId);

			EIMSearchConditionGroup condsAttr = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			// Date型属性に関しては、条件設定が「yyyy/MM/dd」フォーマットのため、範囲指定検索を行う
			if(tmpAttrType.getValueType().getId() == new EIMValueType(sess, EIMValueType.DATE).getId())
			{
				// 属性名が同じ属性値に関しては、全てOR検索を行う
				conds.addCondition(setSearchConditionAttrForDate(sess, condsAttr, attrValueList, tmpAttrType));
			}
			// その他の属性タイプに関しては、通常の比較検索を行う
			else
			{
				// 属性名が同じ属性値に関しては、全てOR検索を行う
				conds.addCondition(setSearchConditionAttrForOther(sess, condsAttr, attrValueList, tmpAttrType));
			}
		}

		return conds;
	}

	/**
	 * 日付型属性について、範囲指定の検索条件を追加する
	 * @param sess
	 * @param condsAttr
	 * @param attrValueList
	 * @param attrType
	 * @return
	 * @throws Exception
	 */
	private static EIMSearchConditionGroup setSearchConditionAttrForDate(EIMSession sess, EIMSearchConditionGroup condsAttr, List<String> attrValueList, EIMAttributeType attrType) throws Exception
	{
		DateFormat df = DateFormat.getInstance();
		for(String attrValue : attrValueList)
		{
			EIMSearchConditionRange cond = new EIMSearchConditionRange(
					EIMSearchOperatorEnum.OR,									// 前方演算子「OR」
					attrType,													// 属性「(引数にて指定された属性)」
					EIMSearchOperatorEnum.GE,									// 比較演算子「<=」
					df.parse(attrValue + EIMCommandConstant.FIND_COMMAND_DATE_FROM),	// 値「(引数にて指定された属性値) + 00:00:00」
					EIMSearchOperatorEnum.LE,									// 比較演算子「>=」
					df.parse(attrValue + EIMCommandConstant.FIND_COMMAND_DATE_TO)			// 値「(引数にて指定された属性値) + 23:59:59」
					);

			// 検索条件に追加する
			condsAttr.addCondition(cond);
		}

		return condsAttr;
	}

	/**
	 * 日付型属性以外の属性について、範囲指定の検索条件を追加する
	 * @param sess
	 * @param condsAttr
	 * @param attrValueList
	 * @param attrType
	 * @return
	 * @throws Exception
	 */
	private static EIMSearchConditionGroup setSearchConditionAttrForOther(EIMSession sess, EIMSearchConditionGroup condsAttr, List<String> attrValueList, EIMAttributeType attrType) throws Exception
	{
		for(String attrValue : attrValueList)
		{
			SearchCondition cond;
			// 数値型、実数型属性はLIKEでなくEQUAL比較でないといけない
			if (attrType.getValueType().getId() == EIMValueType.DOUBLE){
				cond = new EIMSearchConditionCompare(
						EIMSearchOperatorEnum.OR,	// 前方演算子「OR」
						attrType,					// 属性「(引数にて指定された属性)」
						EIMSearchOperatorEnum.EQ,	// 比較演算子「=」
						Double.parseDouble(attrValue));
			} else if (attrType.getValueType().getId() == EIMValueType.INTEGER){
				cond = new EIMSearchConditionCompare(
						EIMSearchOperatorEnum.OR,	// 前方演算子「OR」
						attrType,					// 属性「(引数にて指定された属性)」
						EIMSearchOperatorEnum.EQ,	// 比較演算子「=」
						Integer.parseInt(attrValue));
			} else{
				cond = new EIMSearchConditionLike(
						EIMSearchOperatorEnum.OR,	// 前方演算子「OR」
						attrType,					// 属性「(引数にて指定された属性)」
						EIMSearchOperatorEnum.LIKE,	// 比較演算子「like」
						attrValue);
			}

			// 検索条件に追加する
			condsAttr.addCondition(cond);

		}

		return condsAttr;
	}



	/**
	 * 返却項目の設定を行う
	 * @param sess
	 * @param selectTarget
	 */
	@SuppressWarnings("unchecked")
	private static void setResultAttrs(EIMSession sess, EIMSearchSelectEIMObject selectTarget) throws Exception
	{
		List resultList = new ArrayList();

		// オブジェクトを追加
		for(EIMAttributeType attrType : FIND_COMMAND_DEFAULT_OBJ_TYPE_ATTRTYPE)
		{
			resultList.add(attrType);
		}

		// 属性を追加
		for(String attrName : EIMCommandConstant.FIND_COMMAND_DEFAULT_ATTRIBUTE_NAMES)
		{
			resultList.add(AttributeUtils.getAttributeTypeByName(sess, attrName));
		}

	}
	
	/**
	 * WF無しドキュメント
	 * @param sess
	 * @param objList (WFなしドキュメント)
	 * @param noWFOnRevisionList　WF無しステータス「改定中」
	 * @param noWFPubList　WF無しステータス「公開済」
	 * @param limit 検索件数の上限
	 * @return result　改定中ステータスと公開済ステータスの検索結果リストを格納したインスタンス
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static ResultNoWFStsObjList searchObjForNoWFSts(EIMSession sess, List<EIMObject> objList, int limit) throws Exception
	{
		long[] idArray = new long[objList.size()];
		for(int i = 0 ; i < objList.size(); i++)
		{
			idArray[i] = objList.get(i).getId();
		}
		
		EIMAttributeType objId = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID;

		//以下の3つの条件作成1～3は「noWFDocListCondとnoWFDocPubListCond」「noWFDocListCondとnoWFDocOnRevListCond」
		//で2つにまとめられるが、可読性を考慮し、分けて記述した

		// 条件作成1 オブジェクトリスト全体
		EIMSearchConditionIn noWFDocListCond = new EIMSearchConditionIn(EIMSearchOperatorEnum.AND,
				objId,
				EIMSearchOperatorEnum.IN,
				TypeConvertUtils.convertToBuildTypeArray(idArray));

		// 条件作成2 WF無しドキュメント(公開済)
		EIMSearchConditionIn noWFDocPubListCond = new EIMSearchConditionIn(EIMSearchOperatorEnum.AND,
				objId,
				EIMSearchOperatorEnum.IN,
				EIMCommandConstant.NOWF_PUBLIC_OBJSQL);

		// 条件作成3 WF無しドキュメント(改定中)
		EIMSearchConditionIn noWFDocOnRevListCond = new EIMSearchConditionIn(EIMSearchOperatorEnum.AND,
				objId,
				EIMSearchOperatorEnum.IN,
				EIMCommandConstant.NOSTATAS_PUBLIC_EDIT_OBJSQL);

		// WF無し公開済
		EIMSearchConditionGroup noWFPubConds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
		noWFPubConds.addCondition(noWFDocListCond);
		noWFPubConds.addCondition(noWFDocPubListCond);

		// WF無し改定中
		EIMSearchConditionGroup noWFOnRevConds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);
		noWFOnRevConds.addCondition(noWFDocListCond);
		noWFOnRevConds.addCondition(noWFDocOnRevListCond);
		
		// 検索条件項目にセット
		EIMSearchSelectEIMObject selectTargetNoWFPub = new EIMSearchSelectEIMObject();
		EIMSearchSelectEIMObject selectTargetNoWFOnRev = new EIMSearchSelectEIMObject();
		selectTargetNoWFPub.setCondition(noWFPubConds);
		selectTargetNoWFOnRev.setCondition(noWFOnRevConds);
		
		// 検索実行&結果格納
		ResultNoWFStsObjList result = new ResultNoWFStsObjList();
		
		result.setSearchResult((List<EIMObject>)SearchUtils.searchObjects(sess, selectTargetNoWFPub, new EIMSearchLimitCountCondition(limit,true)), 
					  (List<EIMObject>)SearchUtils.searchObjects(sess, selectTargetNoWFOnRev, new EIMSearchLimitCountCondition(limit,true)));

		
		return result;
	}


}
