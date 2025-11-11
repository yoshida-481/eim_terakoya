package common.util;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import app.document.search.EIMDocSearchType;
import app.document.search.condition.EIMDocObjSearchConditionFactory;
import app.document.search.condition.EIMDocRelSearchConditionFactory;
import app.document.search.condition.EIMDocSearchConditionMaker;
import eim.bo.EIMAttributeCondition;
import eim.bo.EIMAttributeType;
import eim.bo.EIMDateRange;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMObjectType;
import eim.bo.EIMRelation;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchOperator;
import eim.bo.EIMSearchPsedoAttributeTypeProcessedFuncTrunc;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMRelation;
import eim.bo.EIMStatusType;
import eim.bo.EIMUser;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.TypeConvertUtils;
import eim.util.internal.search.SearchCondition;
import eim.util.internal.search.SearchConditionCompareAbs;
import eim.util.internal.search.sql.SearchJavaUtil;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class AppSearchUtils  {
	/**
	 *
	 * 指定した条件でオブジェクトを検索します。 <br>
	 * 日付の指定はjava.util.Dateオブジェクトを使用します。
	 * <p>
	 * 下位互換性のため、日付指定では時分秒を無視するので、<br>
	 * 本APIではタイムゾーンに対応していません。
	 *
	 * @param sess
	 *            セッション
	 * @param objType
	 *            オブジェクトタイプ
	 * @param objName
	 *            オブジェクト名。引数文字列をLike条件で検索します。<br>
	 *            引数文字列に含まれる%と_はSQLワイルドカードになります。<br>
	 *            エスケープ文字はありません。
	 * @param subClasses
	 *            サブクラス再帰検索<br>
	 *            (true:子オブジェクトタイプも対象 false:指定されたオブジェクトタイプのみ)
	 * @param latest
	 *            最新履歴のみ検索
	 * @param rev
	 *            レビジョン
	 * @param createUser
	 *            作成ユーザ
	 * @param createDate
	 *            作成日
	 * @param modifyUser
	 *            更新ユーザ
	 * @param modifyDate
	 *            更新日
	 * @param lockUser
	 *            ロックユーザ
	 * @param lockDate
	 *            ロック日
	 * @param attCondList
	 *            属性条件
	 * @param statusTypeList
	 *            ステータス条件
	 * @param statusAttList
	 *            ステータス属性条件（現在未使用）
	 * @param resultOrder
	 *            ソート（現在未使用）
	 * @param accessRoleId
	 * 			  権限ロールID
	 * @throws EIMException
	 *             ストアドプロシージャで例外が発生した場合
	 * @throws EIMException
	 *             メッセージコード2300 検索上限オーバー:e.params[0]にヒット件数が、[1]に上限件数がセットされる
	 * @throws Exception
	 *             その他例外が発生した場合
	 *
	 * @return 検索結果オブジェクト一覧（EIMObject）
	 *
	 */
	@SuppressWarnings("unchecked")
	public static List searchObject(EIMSession sess, EIMObjectType objType,
			String objName, boolean subClasses, boolean latest, int rev,
			EIMUser createUser, java.util.Date createDate, EIMUser modifyUser,
			java.util.Date modifyDate, EIMUser lockUser,
			java.util.Date lockDate, List attCondList, List statusTypeList,
			List statusAttList, String resultOrder,int accessRoleId) throws Exception {
		EIMDateRange createDateRange = null;
		if (createDate != null)
			createDateRange = new EIMDateRange("=", createDate, null, null);

		EIMDateRange modifyDateRange = null;
		if (modifyDate != null)
			modifyDateRange = new EIMDateRange("=", modifyDate, null, null);

		EIMDateRange lockDateRange = null;
		if (lockDate != null)
			lockDateRange = new EIMDateRange("=", lockDate, null, null);

		return searchObject(sess, objType, objName, subClasses, latest, rev,
				createUser, createDateRange, modifyUser, modifyDateRange,
				lockUser, lockDateRange, attCondList, statusTypeList,
				statusAttList,accessRoleId);
	}
	/**
	 *
	 * 指定した条件でオブジェクトを検索します。 <br>
	 * 日付の指定はEIMDateRangeオブジェクトを使用します。
	 * <p>
	 * 検索上限件数は、Config設定SEARCH.RESULT.MAXの値が使用されます。<br>
	 * 下位互換性のため、EIMDateRange日付指定では時分秒を無視するので、<br>
	 * 本APIではタイムゾーンに対応していません。
	 *
	 * @param sess
	 *            セッション
	 * @param objType
	 *            オブジェクトタイプ
	 * @param objName
	 *            オブジェクト名
	 * @param subClasses
	 *            サブクラス再帰検索<br>
	 *            (true:子オブジェクトタイプも対象 false:指定されたオブジェクトタイプのみ)
	 * @param latest
	 *            最新履歴のみ検索
	 * @param rev
	 *            レビジョン
	 * @param createUser
	 *            作成ユーザ
	 * @param createDateRange
	 *            作成期間
	 * @param modifyUser
	 *            更新ユーザ
	 * @param modifyDateRange
	 *            更新期間
	 * @param lockUser
	 *            ロックユーザ
	 * @param lockDateRange
	 *            ロック期間
	 * @param attCondList
	 *            属性条件
	 * @param statusTypeList
	 *            ステータス条件
	 * @param statusAttList
	 *            ステータス属性条件（現在未使用）
	 * @param accessRoleId
	 * 			  権限ロールID
	 * @throws EIMException
	 *             ストアドプロシージャで例外が発生した場合
	 * @throws EIMException
	 *             メッセージコード2300 検索上限オーバー:e.params[0]にヒット件数が、[1]に上限件数がセットされる
	 * @throws Exception
	 *             その他例外が発生した場合
	 *
	 * @return 検索結果オブジェクト一覧（EIMObject）
	 *
	 */
	@SuppressWarnings("unchecked")
	public static EIMSearchResultList searchObject(EIMSession sess,
			EIMObjectType objType, String objName, boolean subClasses,
			boolean latest, int rev, EIMUser createUser,
			EIMDateRange createDateRange, EIMUser modifyUser,
			EIMDateRange modifyDateRange, EIMUser lockUser,
			EIMDateRange lockDateRange, List attCondList, List statusTypeList,
			List statusAttList,int accessRoleId) throws Exception
	{

		// 検索条件をEIMSearchSelectEIMObject条件に変換する
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();

		if(accessRoleId != 0)
			selectTarget.setRole(accessRoleId);

		EIMSearchConditionGroup conds = AppSearchUtils.createEIMObjSearchConds(sess, h,
				objType, objName, subClasses, latest, rev, createUser,
				createDateRange, modifyUser, modifyDateRange, lockUser,
				lockDateRange, attCondList, statusTypeList);
		selectTarget.setCondition(conds);

		return SearchUtils.searchObjects(sess, selectTarget, null);
	}
	/**
	 *
	 * 旧API検索条件をEIMSearchCondXXX検索条件に変換します。 <br>
	 * 日付の指定はEIMDateRangeオブジェクトを使用します。
	 *
	 * @param sess
	 *            セッション
	 * @param h
	 *            条件作成ヘルパー
	 * @param objType
	 *            オブジェクトタイプ
	 * @param objName
	 *            オブジェクト名
	 * @param subClasses
	 *            サブクラス再帰検索(true:子オブジェクトタイプも対象 false:指定されたオブジェクトタイプのみ)
	 * @param latest
	 *            最新履歴のみ検索
	 * @param rev
	 *            レビジョン
	 * @param createUser
	 *            作成ユーザ
	 * @param createDateRange
	 *            作成期間
	 * @param modifyUser
	 *            更新ユーザ
	 * @param modifyDateRange
	 *            更新期間
	 * @param lockUser
	 *            ロックユーザ
	 * @param lockDateRange
	 *            ロック期間
	 * @param attCondList
	 *            属性条件
	 * @param statusTypeList
	 *            ステータス条件
	 *
	 * @throws EIMException
	 *             ストアドプロシージャで例外が発生した場合
	 * @throws Exception
	 *             その他例外が発生した場合
	 *
	 * @return 変換した検索条件
	 *
	 */
	@SuppressWarnings("unchecked")
	private static EIMSearchConditionGroup createEIMObjSearchConds(
			EIMSession sess,
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h,
			EIMObjectType objType, String objName, boolean subClasses,
			boolean latest, int rev, EIMUser createUser,
			EIMDateRange createDateRange, EIMUser modifyUser,
			EIMDateRange modifyDateRange, EIMUser lockUser,
			EIMDateRange lockDateRange, List attCondList, List statusTypeList)
			throws Exception {
		EIMSearchConditionGroup conds = h.group(h.opAnd());
		// Object Type And Sub Classes
		if (objType != null) {
			if (!subClasses)
				conds.addCondition(h.eq(h.opAnd(),
						EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE,
						objType.getId()));
			else
				conds.addCondition(h.eqObjTypeWithSubClasses(h.opAnd(), objType
						.getId(), sess));
		}

		// Name
		if (objName != null && objName.length() > 0)
			conds.addCondition(h.like(h.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME,
					h.opLike(), objName).setIsValueDirectSql(true));

		// Latest
		if (latest)
			conds.addCondition(h.latest(h.opAnd()));
		else if (rev != SearchUtils.NOT_SPECIFIED)
			conds.addCondition(h.eq(h.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.REV, rev));

		// Create User
		if (createUser != null)
			conds.addCondition(h.eq(h.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.CUSER,
					createUser.getId()));

		// Create Date
		conds.addCondition(createDateRangeCondition(createDateRange,
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.CDATE, h));

		// Modify User
		if (modifyUser != null)
			conds.addCondition(h.eq(h.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.MUSER,
					modifyUser.getId()));

		// Modify Date
		conds.addCondition(createDateRangeCondition(modifyDateRange,
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.MDATE, h));

		// Lock User
		if (lockUser != null)
			conds.addCondition(h.eq(h.opAnd(),
					EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.LUSER,
					lockUser.getId()));

		// Lock Date
		conds.addCondition(createDateRangeCondition(lockDateRange,
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.LDATE, h));

		// Attribute
		if (attCondList != null && attCondList.size() > 0) {
			EIMSearchConditionGroup attrConds = h.group(h.opAnd());

			for (Iterator i = attCondList.iterator(); i.hasNext();) {
				EIMAttributeCondition attrCond = (EIMAttributeCondition) i
						.next();

				// Operate
				EIMSearchOperator.BoolRs.AndOr fwdOpe = (attrCond.getOperate() != null) ? (EIMSearchOperator.BoolRs.AndOr) EIMSearchOperator
						.getOperator(attrCond.getOperate())
						: h.opAnd();

				switch (attrCond.getType().getValueType().getId()) {
				case EIMValueType.INTEGER:
					// Integer
					attrConds.addCondition(h.eq(fwdOpe, attrCond.getType(),
							attrCond.getInt()));
				case EIMValueType.DOUBLE:
					// Double
					attrConds.addCondition(h.eq(fwdOpe, attrCond.getType(),
							attrCond.getDouble()));
					break;
				case EIMValueType.STRING:
				case EIMValueType.TEXT:
					// String
					// Text
					attrConds.addCondition(h.like(fwdOpe, attrCond.getType(),
							h.opLike(), attrCond.getString())
							.setIsValueDirectSql(true));
					break;
				case EIMValueType.DATE:
					// Date
					EIMSearchOperator.BoolRs.Compare opeDate = (attrCond
							.getDateOperate() != null) ? (EIMSearchOperator.BoolRs.Compare) EIMSearchOperator
							.getOperator(attrCond.getDateOperate())
							: h.opEq();
					attrConds.addCondition(new EIMSearchConditionCompare(
							fwdOpe,
							new EIMSearchPsedoAttributeTypeProcessedFuncTrunc(
									attrCond.getType()), opeDate, attrCond
									.getDate()));
				}
			}
			conds.addCondition(attrConds);
		}

		// Status
		if (statusTypeList != null && statusTypeList.size() > 0) {
			long[] stTypeIds = new long[statusTypeList.size()];
			for (int i = 0; i < stTypeIds.length; i++) {
				stTypeIds[i] = ((EIMStatusType) statusTypeList.get(i)).getId();
			}
			conds
					.addCondition(h
							.in(
									h.opAnd(),
									EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS_TYPE,
									h.opIn(),TypeConvertUtils.convertToBuildTypeArray(stTypeIds)));
		}

		return conds;
	}
	/**
	 * 旧検索条件EIMDateRangeを新検索条件に変換する
	 *
	 * @param dateRange
	 *            日付範囲
	 * @param attrType
	 *            属性タイプ
	 * @param h
	 *            条件生成ヘルパー
	 * @return 新検索条件
	 */
	private static SearchCondition createDateRangeCondition(
			EIMDateRange dateRange, EIMAttributeType attrType,
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h) {
		EIMSearchConditionGroup conds = new EIMSearchConditionGroup(h.opAnd());
		if (dateRange == null)
			return null;
		if (dateRange.getFromDate() != null) {
			Date dateFrom = SearchJavaUtil.truncateYMD(dateRange.getFromDate());
			if ("=".equals(dateRange.getFromOperate()))
				conds.addCondition(h.eq(h.opAnd(),
						new EIMSearchPsedoAttributeTypeProcessedFuncTrunc(
								attrType), dateFrom));
			else
				conds.addCondition(new EIMSearchConditionCompare(h.opAnd(),
						attrType,
						(EIMSearchOperator.BoolRs.Compare) EIMSearchOperator
								.getOperator(dateRange.getFromOperate()),
						dateFrom));
		}
		if (dateRange.getToDate() != null) {
			Date dateTo = SearchJavaUtil.truncateYMD(dateRange.getToDate());
			conds.addCondition(new EIMSearchConditionCompare(h.opAnd(),
					attrType,
					(EIMSearchOperator.BoolRs.Compare) EIMSearchOperator
							.getOperator(dateRange.getToOperate()), dateTo));
		}
		switch (conds.getConditions().size()) {
		case 0:
			return null;
		case 1:
			return (SearchConditionCompareAbs) conds.getConditions().get(0);
		default:
			return conds;
		}
	}
	
	/**
	 * リストビューに表示されるオブジェクトタイプの検索条件を作成
	 * @param sess セッション情報
	 * @return 「ドキュメント」「フォルダ」「タグ」とそのサブクラスを対象とする検索条件
	 * @throws Exception
	 */
	public static EIMSearchConditionIn getListViewItemObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, true, true, true);
	}
	
	
	/**
	 * 属性ツリービューに表示されるオブジェクトタイプの検索条件を作成
	 * @param sess セッション情報
	 * @return 「ドキュメント」「フォルダ」とそのサブクラスを対象とする検索条件
	 * @throws Exception
	 */
	public static EIMSearchConditionIn getAttrTreeItemObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, true, true ,false);
	}
	
	/**
	 * 承認対象となるオブジェクトタイプについての検索条件を作成
	 * @param sess セッション情報
	 * @return 「ドキュメント」「フォルダ」とそのサブクラスを対象とする検索条件
	 * @throws Exception
	 */
	public static EIMSearchConditionIn getApprovalItemObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, true, true, false);
	}
	
	/**
	 * チェックアウト一覧取得時の検索条件を作成
	 * @param sess セッション情報
	 * @return 「ドキュメント」及びそのサブクラスを対象とする検索条件
	 * @throws Exception
	 */
	public static EIMSearchConditionIn getCheckoutItemObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, true, false, false);
	}
	
	/**
	 * お気に入り表示時のオブジェクトタイプ検索条件を作成
	 * @param sess セッション情報
	 * @return 「フォルダ」及びそのサブクラスを対象とする検索条件
	 * @throws Exception
	 */
	public static EIMSearchConditionIn getMyFavoriteItemObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, false, true, false);
	}

	/**
	 * ツリービューに表示するオブジェクトタイプ検索条件を作成(タグのみ）
	 * @param sess セッション情報
	 * @return 「タグ」とそのサブクラスを対象とする検索条件
	 * @throws Exception
	 */
	public static EIMSearchConditionIn getTreeViewItemTagObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, false, false, true);
	}
	
	/**
	 * ツリービューに表示するオブジェクトタイプ検索条件を作成(フォルダのみ）
	 * @param sess セッション情報
	 * @return 「フォルダ」とそのサブクラスを対象とする検索条件
	 * @throws Exception
	 */
	public static EIMSearchConditionIn getTreeViewItemFolderObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, false, true, false);
	}
	
	/**
	 * 検索リストビューに表示する親オブジェクトタイプ検索条件を作成(フォルダのみ）
	 * @param sess セッション情報
	 * @return 「フォルダ」とそのサブクラスを対象とする検索条件
	 * @throws Exception
	 */
	public static EIMSearchConditionIn getSearchListViewItemFolderObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, false, true, false);
	}
	
	public static EIMSearchConditionIn getTreeViewItemObjTypeCondition(EIMSession sess) throws Exception
	{
		return getDocItemObjTypeConditionInternal(sess, false, true, true);
	}
	
	/**
	 * 検索条件の作成
	 * @param sess セッション情報
	 * @param includeDoc ドキュメントを検索対象に含めるか
	 * @param includeFol タグを検索対象に含めるか
	 * @param includeTag フォルダを検索対象に含めるか
	 * @return オブジェクトタイプに関する検索条件を返却
	 * @throws Exception
	 */
	private static EIMSearchConditionIn getDocItemObjTypeConditionInternal(EIMSession sess, 
			boolean includeDoc, boolean includeFol, boolean includeTag) throws Exception
	{
		EIMSearchSelectEIMObject.SearchConditionBuildHelper helper = 
			new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		
		// 検索対象オブジェクトタイプの取得
		List<EIMObjectType> destTypeList = new ArrayList<EIMObjectType>();
		if (includeDoc) {
			destTypeList.addAll(getObjTypeList(sess, "OBJECT_TYPE_NAME_DOCUMENT"));
		}
		if (includeFol) {
			destTypeList.addAll(getObjTypeList(sess, "OBJECT_TYPE_NAME_FOLDER"));
		}
		if (includeTag) {
			destTypeList.addAll(getObjTypeList(sess, "OBJECT_TYPE_NAME_TAG"));
		}
		
		// IN条件の作成
		long[] types = new long[destTypeList.size()];
		for (int i = 0; i < destTypeList.size(); i++) {
			types[i] = destTypeList.get(i).getId();
		}
		
		return helper.in(helper.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE, 
				helper.opIn(), TypeConvertUtils.convertToBuildTypeArray(types));
	}
	
	/**
	 * 指定したオブジェクトタイプとそのサブクラスを取得する
	 * @param sess セッション情報
	 * @param key コンフィグキー
	 * @return 指定したキーのオブジェクトタイプとそのサブクラスのタイプをリストにして返却
	 * @throws Exception オブジェクトが見つからない、リソースが見つからない、等
	 */
	private static List<EIMObjectType> getObjTypeList(EIMSession sess, String key) throws Exception
	{
		EIMObjectType type = ObjectUtils.getObjectTypeByName(sess, EIMConfig.get(key));
		List<EIMObjectType> result = new ArrayList<EIMObjectType>();
		ObjectUtils.getChildObjectTypeListRecurrently(sess, type, result);
		result.add(0, type);
		
		return result;
	}
	
	/**
	 * 検索条件定義クラス経由のオブジェクト検索
	 * @param sess セッション情報
	 * @param type 検索種別
	 * @param accessRole 権限
	 * @param limitCond 検索件数の上限
	 * @param userData 検索条件定義に利用する任意のデータ
	 * @return 検索結果
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static List<EIMObject> searchObjectsByConditionMaker(EIMSession sess, 
			EIMDocSearchType type , int accessRole, EIMSearchLimitCountCondition limitCond,
			Object userData)
	throws Exception
	{
		List<EIMObject> searchResult = new ArrayList<EIMObject>();
		boolean sessPutFlag = false;
		
		try{
			// JSPからの呼び出しだとEIMThreadContextにセッション情報が格納されてないのでここで入れておく
			if(EIMThreadContext.getEIMSession() == null){
				EIMThreadContext.putEIMSession(sess);
				sessPutFlag = true;
			}

			// 検索種別をスレッドローカルに保管
			EIMThreadContext.put(AppConstant.SEARCH_CONDITION_EMBEDDED_TYPE, type);

			EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
			EIMDocSearchConditionMaker condMaker = 
				EIMDocObjSearchConditionFactory.getConditionMaker(type, userData);
			EIMSearchConditionGroup conds = (condMaker != null) ? condMaker.getCondition() : null;

			if (conds != null && conds.getConditions().size() > 0) {
				selectTarget.setCondition(conds);
				selectTarget.setRole(accessRole);
				searchResult = SearchUtils.searchObjects(sess, selectTarget, limitCond);
			}
		}catch(Exception e){
			throw e;
		}finally{
			if(sessPutFlag){
				// スレッドローカルのクリア
				EIMThreadContext.removeEIMSession();
				sessPutFlag = false;
			}
			// スレッドローカル変数のクリア
			if(EIMThreadContext.get(AppConstant.SEARCH_CONDITION_EMBEDDED_TYPE) != null){
				EIMThreadContext.remove(AppConstant.SEARCH_CONDITION_EMBEDDED_TYPE);
			}
		}
		return searchResult;
	}
	
	
	/**
	 * 検索条件定義クラス経由のリレーション検索
	 * @param sess セッション情報
	 * @param type 検索種別
	 * @param accessRole オブジェクトに対する権限。親子とも同じ値を設定する
	 * @param limitCond 検索件数の上限
	 * @param userData 検索条件定義に利用する任意のデータ
	 * @return 検索結果
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static List<EIMRelation> searchRelationByConditionMaker(EIMSession sess,
			EIMDocSearchType type, int accessRole, EIMSearchLimitCountCondition limitCond,
			Object userData)
	throws Exception
	{
		return searchRelationByConditionMaker(sess, type, accessRole, accessRole, limitCond, userData);
	}

	/**
	 * 検索条件定義クラス経由のリレーション検索
	 * @param sess セッション情報
	 * @param type 検索種別
	 * @param accessRoleParent 親オブジェクトに対する権限
	 * @param accessRoleChild 子オブジェクトに対する権限
	 * @param limitCond 検索件数の上限
	 * @param userData 検索条件定義に利用する任意のデータ
	 * @return 検索結果
	 * @throws Exception
	 */
	@SuppressWarnings("unchecked")
	public static List<EIMRelation> searchRelationByConditionMaker(EIMSession sess,
			EIMDocSearchType type, int accessRoleParent, int accessRoleChild, EIMSearchLimitCountCondition limitCond,
			Object userData)
	throws Exception
	{
		boolean putSessFlag = false;
		
		try{
			// JSPからの呼び出しだとEIMThreadContextにセッション情報が格納されてないのでここで入れておく
			if(EIMThreadContext.getEIMSession() == null){
				EIMThreadContext.putEIMSession(sess);
				putSessFlag = true;
			}

			// 検索種別をスレッドローカルに保管
			EIMThreadContext.put(AppConstant.SEARCH_CONDITION_EMBEDDED_TYPE, type);

			// リレーション、親、子、それぞれの検索条件を入手
			EIMDocSearchConditionMaker relationCondMaker =
				EIMDocRelSearchConditionFactory.getRelConditionMaker(type, userData);
			relationCondMaker.setConditionTarget(2);
			EIMSearchConditionGroup relationConds = relationCondMaker.getCondition();

			EIMDocSearchConditionMaker parentMaker =
				EIMDocRelSearchConditionFactory.getParentConditionMaker(type, userData);
			parentMaker.setConditionTarget(3);
			parentMaker.setMainCondition(relationConds);
			EIMSearchConditionGroup parentConds = parentMaker.getCondition();

			EIMDocSearchConditionMaker childMaker = 
				EIMDocRelSearchConditionFactory.getChildConditionMaker(type, userData);
			childMaker.setConditionTarget(4);
			childMaker.setMainCondition(relationConds);
			EIMSearchConditionGroup childConds = childMaker.getCondition();

			// selectTargetを作成
			EIMSearchSelectEIMRelation relationTarget = new EIMSearchSelectEIMRelation();
			relationTarget.setCondition(relationConds);

			EIMSearchSelectEIMObject parentTarget = new EIMSearchSelectEIMObject();
			parentTarget.setCondition(parentConds);

			EIMSearchSelectEIMObject childTarget = new EIMSearchSelectEIMObject();
			childTarget.setCondition(childConds);

			// Access Roleの設定
			parentTarget.setRole(accessRoleParent);
			childTarget.setRole(accessRoleChild);
			return SearchUtils.searchRelations(sess, relationTarget, parentTarget, childTarget, limitCond);

		}catch(Exception e){
			throw e;
		}finally{
			if(putSessFlag){
				// スレッドローカルのクリア
				EIMThreadContext.removeEIMSession();
				putSessFlag = false;
			}
			// スレッドローカル変数のクリア
			if(EIMThreadContext.get(AppConstant.SEARCH_CONDITION_EMBEDDED_TYPE) != null){
				EIMThreadContext.remove(AppConstant.SEARCH_CONDITION_EMBEDDED_TYPE);
			}
		}
	}
	
}
