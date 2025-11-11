/**
 * 
 */
package app.document.search.condition.impl;

import java.sql.Date;
import java.util.List;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocSearchConditionMaker;
import common.bo.AttributeTree;
import common.bo.AttributeTreeItem;
import common.util.AppConstant;
import common.util.AttributeTreeUtil;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSecurity;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.SecurityUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

/**
 * @author z1J5161
 *
 */
public class DspAttrObjTreeConditionMaker extends EIMDocSearchConditionMaker {
	
	/** 属性ツリー情報 */
	private AttributeTree attrTree;
	
	/** 属性ツリーの上位の属性値を最上位から順に設定する配列 */
	private List<Object> upperAttributeValList;

	/**
	 * @param type
	 * @param userData
	 */
	public DspAttrObjTreeConditionMaker(EIMDocSearchType type, Object userData) {
		super(type, userData);
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#getDefaultCondition()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public EIMSearchConditionGroup getDefaultCondition() throws Exception {
		
		EIMSession sess = EIMThreadContext.getEIMSession();
		return this.getAttrTreeObjSearchCondition(sess, this.getAttrTree(), this.getUpperAttributeValList());
	}
	
	/**
	 * 属性ツリーでのオブジェクト一覧を取得する為の条件を生成
	 * @param sess セッション情報
	 * @param attTree 属性ツリー情報
	 * @param upperAttributeValList 属性の表示順
	 * @return 検索条件
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	private EIMSearchConditionGroup getAttrTreeObjSearchCondition(
			EIMSession sess, AttributeTree attTree, List upperAttributeValList)
			throws Exception {
		// 検索条件グループ作成
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

		// 分類対象別のオブジェクトタイプの取得
		List objTypeList = AttributeTreeUtil.getObjTypeListForCategory(sess, attTree.getClassifyTarget());

		// オブジェクトタイプIDの配列を作成
		long [] objTypes = new long[objTypeList.size()];
		for (int i = 0 ; objTypeList.size() > i ; i++) {
			objTypes[i] = ((EIMObjectType)objTypeList.get(i)).getId();
		}

		// 条件１:対象のオブジェクトタイプを検索
		EIMAttributeType fieldOfObjType = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE;
		EIMSearchConditionIn cond1 = new EIMSearchConditionIn(
				EIMSearchOperatorEnum.AND		//前方演算子OR
				, fieldOfObjType				//TYPE
				, EIMSearchOperatorEnum.IN		// in
				, TypeConvertUtils.convertToBuildTypeArray(objTypes) //'<オブジェクトタイプIDの配列>'
			);
		// 条件１を条件グループに加える
		conds.addCondition(cond1);

		for (int i = 0 ; upperAttributeValList.size() > i ; i++) {

			// 属性ツリー所属属性
			AttributeTreeItem treeItem = (AttributeTreeItem)attTree.getTreeItemList().get(i);

			// 検索条件となる属性値
			Object element = (Object) upperAttributeValList.get(i);

			// 引数属性値を検索条件に設定
			switch (treeItem.getType().getValueType().getId()) {

			// 文字列型の場合
			case EIMValueType.STRING:
				//条件２: and <対象の属性タイプ>が<入力引数>を検索
				if (element != null) {
					EIMSearchConditionCompare condForString = new EIMSearchConditionCompare(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, treeItem.getType()				//<対象の属性タイプ>
							, EIMSearchOperatorEnum.EQ			// ==
							, (String)element					//<入力引数>
						);
					//条件２を条件グループに加える
					conds.addCondition(condForString);
				} else {
					// 当該属性値を保持しないオブジェクトを検索するSQL
					String attrSql =
						"select eo.id from EIMOBJ eo " +
							"where not exists ( " +
								"select * from EIMOBJSTR eos " +
									"where eos.id = eo.id and eos.type = " + treeItem.getType().getId() + ")";
					EIMAttributeType fieldOfObjId = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID;
					EIMSearchConditionIn condForString = new EIMSearchConditionIn(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, fieldOfObjId						//<オブジェクトID>
							, EIMSearchOperatorEnum.IN			// in
							, attrSql							//当該属性値を保持しないオブジェクトID
						);
					//条件２を条件グループに加える
					conds.addCondition(condForString);
				}
				break;

			// 数値型の場合
			case EIMValueType.INTEGER:
				//条件２: and <対象の属性タイプ>が<入力引数>を検索
				if (element != null) {
					EIMSearchConditionCompare condForInt = new EIMSearchConditionCompare(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, treeItem.getType()				//<対象の属性タイプ>
							, EIMSearchOperatorEnum.EQ			// ==
							, ((Long)element).longValue()		//<入力引数>
					);
					//条件２を条件グループに加える
					conds.addCondition(condForInt);
				} else {
					// 当該属性値を保持しないオブジェクトを検索するSQL
					String attrSql =
						"select eo.id from EIMOBJ eo " +
							"where not exists ( " +
								"select * from EIMOBJINT eoi " +
									"where eoi.id = eo.id and eoi.type = " + treeItem.getType().getId() + ")";
					EIMAttributeType fieldOfObjId = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID;
					EIMSearchConditionIn condForInt = new EIMSearchConditionIn(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, fieldOfObjId						//<オブジェクトID>
							, EIMSearchOperatorEnum.IN			// in
							, attrSql							//当該属性値を保持しないオブジェクトID
						);
					//条件２を条件グループに加える
					conds.addCondition(condForInt);
				}
				break;
				// double数値
			case EIMValueType.DOUBLE:
				//<入力引数>を検索
				if (element != null) {
					EIMSearchConditionCompare condForDouble = new EIMSearchConditionCompare(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, treeItem.getType()				//<対象の属・ｽ?・ｽタイ・ｽ?>
							, EIMSearchOperatorEnum.EQ			// ==
							, ((Double)element).doubleValue()		//<入力引数>
					);
					//条件２を条件グループに加える
					conds.addCondition(condForDouble);
				} else {
					// 当該属・ｽ?・ｽ値を保持しな・ｽ?オブジェクトを検索するSQL
					String attrSql =
						"select eo.id from EIMOBJ eo " +
						"where not exists ( " +
						"select * from EIMOBJREAL eor " +
						"where eor.id = eo.id and eor.type = " + treeItem.getType().getId() + ")";
					EIMAttributeType fieldOfObjId = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID;
					EIMSearchConditionIn condForDouble = new EIMSearchConditionIn(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, fieldOfObjId						//<オブジェク・ｽ?ID>
							, EIMSearchOperatorEnum.IN			// in
							, attrSql							//当該属性値を保持しないオブジェクトID
					);
					//条件２を条件グループに加える
					conds.addCondition(condForDouble);
				}
				break;

			// 日付型の場合
			case EIMValueType.DATE:
				//条件２: and <対象の属性タイプ>が<入力引数>を検索
				if (element != null) {
					EIMSearchConditionCompare condForDate = new EIMSearchConditionCompare(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, treeItem.getType()				//<対象の属性タイプ>
							, EIMSearchOperatorEnum.EQ			// ==
							, (Date)element						//<入力引数>
					);
					//条件２を条件グループに加える
					conds.addCondition(condForDate);
				} else {
					// 当該属性値を保持しないオブジェクトを検索するSQL
					String attrSql =
						"select eo.id from EIMOBJ eo " +
							"where not exists ( " +
								"select * from EIMOBJDATE eod " +
									"where eod.id = eo.id and eod.type = " + treeItem.getType().getId() + ")";
					EIMAttributeType fieldOfObjId = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID;
					EIMSearchConditionIn condForDate = new EIMSearchConditionIn(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, fieldOfObjId						//<オブジェクトID>
							, EIMSearchOperatorEnum.IN			// in
							, attrSql							//当該属性値を保持しないオブジェクトID
						);
					//条件２を条件グループに加える
					conds.addCondition(condForDate);
				}
				break;

				/**
				 * テキスト型を属性ツリービューにて表示しない仕様⇒コメントアウト。
				 * */
/*
			// テキスト型の場合
			case EIMValueType.TEXT:
				//条件２: and <対象の属性タイプ>が<入力引数>を検索
				if (element != null) {
					EIMSearchConditionCompare condForText = new EIMSearchConditionCompare(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, treeItem.getType()				//<対象の属性タイプ>
							, EIMSearchOperatorEnum.EQ			// ==
							, (String)element					//<入力引数>
					);
					//条件２を条件グループに加える
					conds.addCondition(condForText);
				} else {
					// 当該属性値を保持しないオブジェクトを検索するSQL
					String attrSql =
						"select eo.id from EIMOBJ eo " +
							"where not exists ( " +
								"select * from EIMOBJTEXT eot " +
									"where eot.id = eo.id and eot.type = " + treeItem.getType().getId() + ")";
					EIMAttributeType fieldOfObjId = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID;
					EIMSearchConditionIn condForText = new EIMSearchConditionIn(
							EIMSearchOperatorEnum.AND			//前方演算子AND
							, fieldOfObjId						//<オブジェクトID>
							, EIMSearchOperatorEnum.IN			// in
							, attrSql							//当該属性値を保持しないオブジェクトID
						);
					//条件２を条件グループに加える
					conds.addCondition(condForText);
				}
				break;
*/
			default:	//それ以外
				break;
			}
		}

		// 読取権限のみの場合
		List readOnlySecurities = SecurityUtils.getReadOnlySecurityList(sess);
		if (readOnlySecurities.size() > 0) {

			long[] readOnlySecurityIds = new long[readOnlySecurities.size()];
			for (int i = 0; i < readOnlySecurityIds.length; i++)
			{
				EIMSecurity sec = (EIMSecurity) readOnlySecurities.get(i);
				readOnlySecurityIds[i] = sec.getId();
			}

			// 検索条件(読取権限用)グループ作成
			EIMSearchConditionGroup condsForReadOnlyAll = new EIMSearchConditionGroup(EIMSearchOperatorEnum.AND);

			// 条件3-1:対象のステータスタイプ種別「公開済み」を検索
			EIMAttributeType fieldOfStatusTypeKind = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS_TYPE_KIND;
			EIMSearchConditionCompare condsForPublic = new EIMSearchConditionCompare(
					EIMSearchOperatorEnum.AND				//前方演算子AND
					, fieldOfStatusTypeKind					//ステータス種別
					, EIMSearchOperatorEnum.EQ				// =
					, AppConstant.STATUS_TYPE_KIND_ID_PUBLIC 	//公開済み
				);

			// 条件3-1を条件(読取権限用)グループに加える
			condsForReadOnlyAll.addCondition(condsForPublic);

			// 条件3-2:readのみ権限以外を検索
			EIMAttributeType fieldOfSecurity = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.SECURITY;
			EIMSearchConditionIn condsForSecurity = new EIMSearchConditionIn(
					EIMSearchOperatorEnum.OR				//前方演算子OR
					, fieldOfSecurity						//セキュリティ
					, EIMSearchOperatorEnum.NOT_IN			// not in
					, TypeConvertUtils.convertToBuildTypeArray(readOnlySecurityIds) //読取権限セキュリティ
				);

			// 条件3-2を条件(読取権限用)グループに加える
			condsForReadOnlyAll.addCondition(condsForSecurity);

			// 条件3-3:ステータスがない、かつ、readのみ権限を検索

			String whereSqlForNotStatus =
					"select id from EIMOBJ " +
					"where status = " + AppConstant.STATUS_TYPE_KIND_ID_NONE + " " +
					"and latest = " + AppConstant.LATEST_HISTORY_FLAG_ON ;

			// 検索条件(ステータスなしかつReadのみ)グループ作成
			EIMSearchConditionGroup condsForNotStatusReadOnly = new EIMSearchConditionGroup(EIMSearchOperatorEnum.OR);

			// 条件3-3-1:ステータスなしを検索
			EIMAttributeType fieldOfObjId = EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID;
			EIMSearchConditionIn condsForNotStatus = new EIMSearchConditionIn(
					EIMSearchOperatorEnum.AND			//前方演算子AND
					, fieldOfObjId						//オブジェクトID
					, EIMSearchOperatorEnum.IN			// in
					, whereSqlForNotStatus			 	//ステータスなしオブジェクトID取得SQL
				);

			// 条件3-3-1を条件(ステータスなしかつReadのみ)グループに加える
			condsForNotStatusReadOnly.addCondition(condsForNotStatus);

			// 条件3-3-2:readのみ権限を検索
			EIMSearchConditionIn condsForReadOnly = new EIMSearchConditionIn(
					EIMSearchOperatorEnum.AND			//前方演算子AND
					, fieldOfSecurity					//オブジェクトID
					, EIMSearchOperatorEnum.IN			// in
					, TypeConvertUtils.convertToBuildTypeArray(readOnlySecurityIds)	//読取権限セキュリティ
				);

			// 条件3-3-2を条件(ステータスなしかつReadのみ)グループに加える
			condsForNotStatusReadOnly.addCondition(condsForReadOnly);


			// 条件3-3を条件(読取権限用)グループに加える
			condsForReadOnlyAll.addCondition(condsForNotStatusReadOnly);


			// 条件(読取権限用)を条件(全体)グループに加える
			conds.addCondition(condsForReadOnlyAll);

		}
		return conds;
	}

	/**
	 * @see app.document.search.condition.EIMDocSearchConditionMaker#parseUserData(java.lang.Object)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void parseUserData(Object userData) throws Exception {
		
		// 配列の一番目がAttrTree：属性ツリー情報
		// 配列の二番目がList(Object)：属性ツリーの上位の属性値を最上位から順に設定する配列
		Object[] data = (Object[])userData;
		this.setAttrTree((AttributeTree)data[0]);
		this.setUpperAttributeValList((List<Object>)data[1]);
	}

	/**
	 * @return the attrTree
	 */
	public AttributeTree getAttrTree() {
		return attrTree;
	}

	/**
	 * @param attrTree the attrTree to set
	 */
	public void setAttrTree(AttributeTree attrTree) {
		this.attrTree = attrTree;
	}

	/**
	 * @return the upperAttributeValList
	 */
	public List<Object> getUpperAttributeValList() {
		return upperAttributeValList;
	}

	/**
	 * @param upperAttributeValList the upperAttributeValList to set
	 */
	public void setUpperAttributeValList(List<Object> upperAttributeValList) {
		this.upperAttributeValList = upperAttributeValList;
	}
	
	

}
