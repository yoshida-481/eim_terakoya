package app.document.search;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jakarta.servlet.http.HttpServletRequest;

import common.util.AppConstant;

import eim.bo.EIMAttributeType;
import eim.bo.EIMObjectType;
import eim.bo.EIMSearchConditionCompare;
import eim.bo.EIMSearchConditionGroup;
import eim.bo.EIMSearchConditionIn;
import eim.bo.EIMSearchConditionLike;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchOperatorEnum;
import eim.bo.EIMSearchPsedoAttributeTypeOtherLang;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSecurity;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.DateUtils;
import eim.util.EIMConfig;
import eim.util.EIMUtils;
import eim.util.ObjectAttributeUtils;
import eim.util.ObjectUtils;
import eim.util.SearchUtils;
import eim.util.SecurityUtils;
import eim.util.StringUtils;

/**
 * aa
 * 
 */
public class ContentSearch2 {
	/**
	 * 検索上限件数
	 */
	private static final int MY_SEARCH_LIMIT_CNT = 1000;

	/**
	 * システム属性名のセット。typeの属性の中からユーザー属性を篩い分けるために使う
	 */
	static final Set excludeAttrTypeNamesInUserExtAttr = new HashSet();
	// システム属性名セットのセットアップ
	static
	{
		excludeAttrTypeNamesInUserExtAttr.add("パス");
		excludeAttrTypeNamesInUserExtAttr.add("プロパティ");
		excludeAttrTypeNamesInUserExtAttr.add("作成者");
		excludeAttrTypeNamesInUserExtAttr.add("有効期限");
	}

	/**
	 * セッション
	 */
	private EIMSession sess;

	/**
	 * HttpRequest
	 */
	private HttpServletRequest request;

	/**
	 * "パス"属性タイプ
	 */
	private EIMAttributeType attrTypeOfPath;

	/**
	 * "プロパティ"属性タイプ
	 */
	private EIMAttributeType attrTypeOfProperty;

	/**
	 * "作成者"属性タイプ
	 */
	private EIMAttributeType attrTypeOfCreateUserId;

	/**
	 * ユーザー属性タイプのリスト
	 */
	private List attrTypesUserExt = new ArrayList();

	/**
	 * 読取専用セキュリティIDのリスト
	 */
	private List readOnlySecurities;

	/**
	 * コンストラクタ
	 * 
	 * @param sess セッション
	 * @param request HttpServletRequest
	 * @throws Exception 予期せぬ例外
	 */
	ContentSearch2(EIMSession sess, HttpServletRequest request) throws Exception {
		this.sess = sess;
		this.request = request;
		readOnlySecurities = SecurityUtils.getReadOnlySecurityList(sess);
	}

	/**
	 * 読取専用セキュリティIDのリストを返す
	 * 
	 * @return 読取専用セキュリティIDのリスト
	 */
	List getReadOnlySecurities()
	{
		return readOnlySecurities;
	}

	/**
	 * 画面パラメータに基づき、検索を行い結果を返す
	 * 
	 * @return EIMSearchObjectsResultListリスト
	 * @throws Exception 検索上限件数を超えた場合、または予期せぬ例外
	 */
	EIMSearchResultList search() throws Exception
	{
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		EIMSearchConditionGroup conds = h.group(h.opAnd());

		{// latest条件
			EIMSearchConditionCompare latestCond = h.latest(h.opAnd());
			conds.addCondition(latestCond);
		}

		EIMObjectType objType = ObjectUtils.getObjectTypeByName(sess, EIMConfig
				.get("OBJECT_TYPE_NAME_DOCUMENT"));
		{// ObjectType条件
			EIMSearchConditionIn objTypeCond = h.eqObjTypeWithSubClasses(h.opAnd(),
					objType.getId(), sess);
			objTypeCond.setHighPriority();
			conds.addCondition(objTypeCond);
		}

		// User Attribute Types only ValueType is String or Text
		setupAttrTypesUserExt(objType);

		{// パス条件
			String prmPathCondition = EIMUtils.getParameter(request, "pathCondition");
			String prmSearchPath = EIMUtils.getParameter(request, "searchPath");
			if (!prmSearchPath.endsWith("/"))
				prmSearchPath += "/";

			if ("true".equals(prmPathCondition) && prmSearchPath != null)
			{
				EIMSearchConditionLike pathCond = h.like(h.opAnd(), getAttrTypeOfPath(),
						h.opLike(), StringUtils.escapeUserWildcardChars(prmSearchPath) + "*");
				pathCond.setHighPriority();
				conds.addCondition(pathCond);
			}
		}

		setupConditionOfStatus(conds, h);// ステータス条件
		setupConditionOfKeyword(conds, h);// キーワード条件
		setupConditionOfDetail(conds, h);// 詳細条件
		setupConditionOfReadOnlySecurity(conds, h);// 読取専用権限条件
		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();
		selectTarget.setCondition(conds);
		selectTarget.setResultAttrs(createResultAttrs());
		return SearchUtils.searchObjects(sess, selectTarget, new EIMSearchLimitCountCondition(
				MY_SEARCH_LIMIT_CNT, true));
	}

	/**
	 * "パス"属性タイプを返す
	 * 
	 * @return "パス"属性タイプ
	 * @throws Exception 予期せぬ例外
	 */
	private EIMAttributeType getAttrTypeOfPath() throws Exception
	{
		if (attrTypeOfPath == null)
			attrTypeOfPath = AttributeUtils.getAttributeTypeByName(sess, "パス");
		return attrTypeOfPath;
	}

	/**
	 * "プロパティ"属性タイプを返す
	 * 
	 * @return "プロパティ"属性タイプ
	 * @throws Exception 予期せぬ例外
	 */
	private EIMAttributeType getAttrTypeOfProperty() throws Exception
	{
		if (attrTypeOfProperty == null)
			attrTypeOfProperty = AttributeUtils.getAttributeTypeByName(sess, "プロパティ");
		return attrTypeOfProperty;
	}

	/**
	 * "作成者"属性タイプを返す
	 * 
	 * @return "作成者"属性タイプ
	 * @throws Exception 予期せぬ例外
	 */
	private EIMAttributeType getAttrTypeOfCreateUserId() throws Exception
	{
		if (attrTypeOfCreateUserId == null)
			attrTypeOfCreateUserId = AttributeUtils.getAttributeTypeByName(sess, "作成者");
		return attrTypeOfCreateUserId;
	}

	/**
	 * ユーザー属性タイプリストをセットアップする
	 * 
	 * @param objType オブジェクトタイプID
	 * @throws Exception 予期せぬ例外
	 */
	private void setupAttrTypesUserExt(EIMObjectType objType) throws Exception
	{
		for (Iterator i = ObjectAttributeUtils.getAttributeTypeList(sess, objType).iterator(); i
				.hasNext();)
		{
			EIMAttributeType attrType = (EIMAttributeType) i.next();
			if (!excludeAttrTypeNamesInUserExtAttr.contains(attrType.getDefName()))
				attrTypesUserExt.add(attrType);
		}

	}

	/**
	 * ステータス検索条件をセットアップする
	 * 
	 * @param conds 条件セット先条件グループ
	 * @param h 条件作成ヘルパー
	 */
	private void setupConditionOfStatus(EIMSearchConditionGroup conds,
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h)
	{
		String prmStatus = request.getParameter("status");
		if (prmStatus != null && !prmStatus.equals("all"))
		{
			// 未公開
			if (prmStatus.equals("edit"))
				conds.addCondition(h.compare(h.opAnd(),
						EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS_TYPE_KIND, h.opNe(),
						AppConstant.STATUS_TYPE_KIND_ID_PUBLIC));
			else if (prmStatus.equals("public"))
				conds.addCondition(h.eq(h.opAnd(),
						EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS_TYPE_KIND,
						AppConstant.STATUS_TYPE_KIND_ID_PUBLIC));
			else if (prmStatus.equals("checkout"))
			{
				conds.addCondition(
						h.eq(h.opAnd(),
								EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS_TYPE_KIND,
								AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)).addCondition(
						h.compare(h.opAnd(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.LUSER,
								h.opGt(), 0));
			}
		}
	}

	/**
	 * キーワード検索条件をセットアップする
	 * 
	 * @param conds 条件セット先条件グループ
	 * @param h 条件作成ヘルパー
	 * @throws Exception 予期せぬ例外
	 */
	private void setupConditionOfKeyword(EIMSearchConditionGroup conds,
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h) throws Exception
	{
		List keywords = StringUtils.getKeywordList(EIMUtils.getParameter(request, "keyword"));
		boolean doContentsSearch = "true".equals(EIMUtils.getParameter(request, "contents"));
		if (keywords.size() > 0)
		{
			List attrs = new ArrayList();
			attrs.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME);// 名称
			attrs.add(getAttrTypeOfPath());// パス
			attrs.add(getAttrTypeOfProperty());// プロパティ

			// ユーザ属性。ただし、タイプDateとIntは対象外
			for (Iterator i = attrTypesUserExt.iterator(); i.hasNext();)
			{
				EIMAttributeType attrType = (EIMAttributeType) i.next();
				switch (attrType.getValueType().getId()) {
				case EIMValueType.DATE:
				case EIMValueType.INTEGER:
					break;
				default:
					attrs.add(attrType);
					break;
				}
			}

			EIMSearchConditionGroup keywordsConds = h.group(h.opAnd());
			for (Iterator i = keywords.iterator(); i.hasNext();)
			{
				String keyword = (String) i.next();
				String keyworkLikeStr = "*" + StringUtils.escapeUserWildcardChars(keyword) + "*";
				EIMSearchConditionGroup keywordConds = h.group(h.opAnd());
				// システム属性＋ユーザー属性(STR,TEXTのみ)
				for (Iterator j = attrs.iterator(); j.hasNext();)
				{
					EIMAttributeType attrType = (EIMAttributeType) j.next();
					keywordConds.addCondition(h
							.like(h.opOr(), attrType, h.opLike(), keyworkLikeStr));
				}

				// 作成者ID経由の作成者名
				keywordConds.addCondition(new EIMSearchConditionIn(h.opOr(),
						getAttrTypeOfCreateUserId(), h.opIn(),
						"select usid from EIMUSEROTHER where lid='"
								+ StringUtils.sqlSanitize(sess.getLangId())
								+ "' and name like '"
								+ StringUtils.sqlSanitize(StringUtils
										.convertUserWildcardToSqlWildcard(keyworkLikeStr))
								+ "' escape '\\'"));

				// OracleText検索
				if (doContentsSearch)
					keywordConds.addCondition(h.matchDocContents(h.opOr(), keyword));

				keywordsConds.addCondition(keywordConds);
			}
			conds.addCondition(keywordsConds);
		}
	}

	/**
	 * 「詳細」検索条件をセットアップする
	 * 
	 * @param conds 条件セット先条件グループ
	 * @param h 条件作成ヘルパー
	 * @throws Exception 予期せぬ例外
	 */
	private void setupConditionOfDetail(EIMSearchConditionGroup conds,
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h) throws Exception
	{
		// プロパティ
		addLikeCondition("property", getAttrTypeOfProperty(), conds, h);

		// 作成者名
		{
			String prmStr = EIMUtils.getParameter(request, "createUserName");
			if (prmStr != null && prmStr.length() > 0)
				conds.addCondition(new EIMSearchConditionIn(h.opAnd(), getAttrTypeOfCreateUserId(),
						h.opIn(), "select usid from EIMUSEROTHER where lid='"
								+ StringUtils.sqlSanitize(sess.getLangId())
								+ "' and name like '"
								+ StringUtils.sqlSanitize(StringUtils
										.convertUserWildcardToSqlWildcard(prmStr))
								+ "' escape '\\'"));
		}
		// 作成日
		addDateRangeCondition("createDateFrom", "createDateTo",
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.CDATE, conds, h);

		addLikeCondition("modifyUserName", new EIMSearchPsedoAttributeTypeOtherLang(
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.MUSER_NAME, sess), conds, h);

		// 更新日
		addDateRangeCondition("modifyDateFrom", "modifyDateTo",
				EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.MDATE, conds, h);

		// ユーザ属性
		for (Iterator i = attrTypesUserExt.iterator(); i.hasNext();)
		{
			EIMAttributeType attrType = (EIMAttributeType) i.next();
			String paramName = "attType_" + attrType.getId();
			switch (attrType.getValueType().getId()) {
			case EIMValueType.DATE: //
			{
				addDateRangeCondition(paramName, paramName, attrType, conds, h);
			}
				break;
			default:
				addLikeCondition(paramName, attrType, conds, h);
				break;
			}
		}
	}

	/**
	 * 指定属性の値が指定リクエストパラメータの値とLike一致する条件をセットアップする
	 * 
	 * @param paramName リクエストパラメータ名
	 * @param attrType 属性タイプ
	 * @param conds 条件セット先条件グループ
	 * @param h 条件作成ヘルパー
	 */
	private void addLikeCondition(String paramName, EIMAttributeType attrType,
			EIMSearchConditionGroup conds, EIMSearchSelectEIMObject.SearchConditionBuildHelper h)
	{
		String prmStr = EIMUtils.getParameter(request, paramName);
		if (prmStr != null && prmStr.length() > 0)
			conds.addCondition(h.like(h.opAnd(), attrType, h.opLike(), prmStr));
	}

	/**
	 * 指定属性の値が指定リクエストパラメータの日付値と範囲一致する条件をセットアップする
	 * 
	 * @param paramNameFrom From値のリクエストパラメータ名
	 * @param paramNameTo To値のリクエストパラメータ名
	 * @param attrType 属性タイプ
	 * @param conds 条件セット先条件グループ
	 * @param h 条件作成ヘルパー
	 * @throws Exception 予期せぬ例外
	 */
	private void addDateRangeCondition(String paramNameFrom, String paramNameTo,
			EIMAttributeType attrType, EIMSearchConditionGroup conds,
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h) throws Exception
	{
		Date dateFrom = null;
		Date dateTo = null;

		String prmStr = EIMUtils.getParameter(request, paramNameFrom);
		if (prmStr != null && prmStr.length() > 0)
			dateFrom = new Date(DateUtils.convCLTzToDBTzTime(sess, StringUtils.getDateFromString(
					sess, prmStr)));

		prmStr = EIMUtils.getParameter(request, paramNameTo);
		if (prmStr != null && prmStr.length() > 0)
		{
			dateTo = new Date(DateUtils.convCLTzToDBTzTime(sess, StringUtils.getDateFromString(
					sess, prmStr)));
			Calendar cal = Calendar.getInstance();
			cal.setTime(dateTo);
			cal.add(Calendar.DATE, 1);
			dateTo = cal.getTime();
		}
		if (dateFrom != null || dateTo != null)
			conds.addCondition(h.range(h.opAnd(), attrType, h.opGe(), dateFrom, h.opLt(), dateTo));
	}

	/**
	 * 「公開済」または「readのみ権限以外」のドキュメントであること、を条件とする
	 * 
	 * @param conds 条件セット先条件グループ
	 * @param h 条件作成ヘルパー
	 * @throws Exception 予期せぬ例外
	 */
	private void setupConditionOfReadOnlySecurity(EIMSearchConditionGroup conds,
			EIMSearchSelectEIMObject.SearchConditionBuildHelper h) throws Exception
	{
		{// Read Only Security条件
			if (readOnlySecurities.size() == 0)
				return;

			int[] readOnlySecurityIds = new int[readOnlySecurities.size()];
			for (int i = 0; i < readOnlySecurityIds.length; i++)
			{
				EIMSecurity sec = (EIMSecurity) readOnlySecurities.get(i);
				readOnlySecurityIds[i] = sec.getId();
			}
			EIMSearchConditionGroup secCond = h.group(h.opAnd());
			secCond.addCondition(
					h.eq(h.opAnd(),
							EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS_TYPE_KIND,
							AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)).addCondition(
					h.in(h.opOr(), EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.SECURITY,
							EIMSearchOperatorEnum.NOT_IN, readOnlySecurityIds));

			conds.addCondition(secCond);
		}
	}

	/**
	 * 検索APIに対する、検索結果のEIMObjectのフィールドにセットしてほしい項目をリストアップする
	 * 
	 * @return EIMObjectのフィールドにセットしてほしい項目のリスト
	 * @throws Exception 予期せぬ例外
	 */
	private List createResultAttrs() throws Exception
	{
		List ret = new ArrayList();
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.ID);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.TYPE);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.NAME);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.REV);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.MUSER);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.MDATE);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.STATUS);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.LUSER);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.LDATE);
		ret.add(EIMSearchSelectEIMObject.PsedoAttributeTypeEnum.SECURITY);
		ret.add(getAttrTypeOfProperty());
		ret.add(getAttrTypeOfPath());
		return ret;
	}
}
