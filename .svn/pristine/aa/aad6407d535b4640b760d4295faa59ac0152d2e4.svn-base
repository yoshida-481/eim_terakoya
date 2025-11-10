package jp.co.ctc_g.eim.app.document.business.domain.search.criteria;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import jp.co.ctc_g.eim.framework2.business.domain.criteria.RangeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;

/**
 * コンテンツ検索条件を表すクライテリアクラスです。
 */
public class ContentSearchCriteria {

	/**
	 * 検索対象種別を表す列挙型です。
	 */
	public enum ContentSearchTypeEnum {

		DOCUMENT("document"),
		FOLDER("folder"),
		TAG("tag");

		/** 検索種別 */
		private final String value;

		/**
		 * コンストラクタです。
		 * @param value
		 * @param typeName
		 */
		private ContentSearchTypeEnum(String value) {
			this.value = value;
		}

		/**
		 * 検索種別を取得します。
		 * @return 検索種別
		 */
		public String getValue() {
			return value;
		}

		/**
		 * 検索種別をキーに列挙値を取得します。
		 * @param value
		 * @return
		 */
		public static ContentSearchTypeEnum getByValue(String value) {
			return Arrays.stream(ContentSearchTypeEnum.values())
	                .filter(item -> item.getValue().equals(value))
	                .findFirst()
	                .orElse(null);
		}

	}

	/**
	 * 検索対象ステータス種別を表す列挙型です。
	 */
	public enum ContentSearchStatusTypeEnum {
		ALL("all"),
		EDIT("edit"),
		PUBLIC("public"),
		CHECKOUT("checkout");

		private final String value;

		private ContentSearchStatusTypeEnum(String value) {
			this.value = value;
		}

		public String getValue() {
			return value;
		}

		public static ContentSearchStatusTypeEnum getByValue(String value) {
			return Arrays.stream(ContentSearchStatusTypeEnum.values())
	                .filter(item -> item.getValue().equals(value))
	                .findFirst()
	                .orElse(null);
		}
	}

	/**
	 * 表示タイプを表す列挙型です。
	 */
	public enum DisplayTypeEnum {
		/** 本文抜粋 */
		TEXT_EXCERPT(0),
		/** リスト */
		LIST(1),
		/** サムネイル */
		THUMBNAIL(2);

		/** ID */
		Integer id = null;

		/**
		 * コンストラクタです。
		 * @param id ID
		 */
		private DisplayTypeEnum(Integer id) {
			this.id = id;
		}

		public static DisplayTypeEnum getById(Integer id) {
			return Arrays.stream(DisplayTypeEnum.values())
					.filter(type -> type.id.equals(id))
					.findFirst().orElse(null);
		}
	}

	// ---------------
	// Property
	// ---------------

	/** 検索種別 */
	private ContentSearchTypeEnum searchType = null;

	/** 検索対象 */
	private String searchPath = null;

	/** 空フォルダ */
	private Boolean emptyFolder = null;

	/** キーワード */
	private String keyword = null;

	/** 全文を含む */
	private Boolean includingFullText = null;

	/** 過去履歴を含む */
	private Boolean includingPastHistory = null;

	/** ステータス種別 */
	private ContentSearchStatusTypeEnum statusType = null;

	/** テーブルID */
	private long tableId = 0;

	/** テーブル定義名 */
	private String tableDefName = null;

	/** 部分一致 */
	private Boolean partialMatch = null;

	/** 属性値検索条件リスト */
	private List<ContentSearchAttributeCriteria> attributeCriteriaList = null;

	/** ファセットによる絞込み条件 */
	private ContentSearchFieldCriteria facetFieldCriteria = null;
	// TODO V6140ではドリルダウンには対応しないためファセットによる絞込み条件は一つとする
//	private List<ContentSearchFieldCriteria> facetFieldCriteriaList = null;

	/** 日時レンジファセットによる絞込み条件 */
	private ContentSearchFieldRangeCriteria<Date> facetFieldRangeDateCriteria = null;

	/** 数値レンジファセットによる絞込み条件 */
	private ContentSearchFieldRangeCriteria<Number> facetFieldRangeNumericCriteria = null;

	/** 表示タイプ */
	private DisplayTypeEnum displayType = null;

	// ---------------
	// getter/setter
	// ---------------

	/**
	 * 検索種別を取得します。
	 * @return 検索種別
	 */
	public ContentSearchTypeEnum getSearchType() {
		return searchType;
	}

	/**
	 * 検索種別を設定します。
	 * @param searchType 検索種別
	 */
	public void setSearchType(ContentSearchTypeEnum searchType) {
		this.searchType = searchType;
	}

	/**
	 * 検索対象を取得します。
	 * @return 検索対象
	 */
	public String getSearchPath() {
		return searchPath;
	}

	/**
	 * 検索対象を設定します。
	 * @param searchPath 検索対象
	 */
	public void setSearchPath(String searchPath) {
		this.searchPath = searchPath;
	}

	/**
	 * 空フォルダを取得します。
	 * @return 空フォルダ
	 */
	public Boolean getEmptyFolder() {
		return emptyFolder;
	}

	/**
	 * 空フォルダを設定します。
	 * @param emptyFolder 空フォルダ
	 */
	public void setEmptyFolder(Boolean emptyFolder) {
		this.emptyFolder = emptyFolder;
	}

	/**
	 * キーワードを取得します。
	 * @return キーワード
	 */
	public String getKeyword() {
		return keyword;
	}

	/**
	 * キーワードを設定します。
	 * @param keyword キーワード
	 */
	public void setKeyword(String keyword) {
		this.keyword = keyword;
	}

	/**
	 * 全文を含むを取得します。
	 * @return 全文を含む
	 */
	public Boolean getIncludingFullText() {
		return includingFullText;
	}

	/**
	 * 全文を含むを設定します。
	 * @param includingFullText 全文を含む
	 */
	public void setIncludingFullText(Boolean includingFullText) {
		this.includingFullText = includingFullText;
	}

	/**
	 * 過去履歴を含むを取得します。
	 * @return 過去履歴を含む
	 */
	public Boolean getIncludingPastHistory() {
		return includingPastHistory;
	}

	/**
	 * 過去履歴を含むを設定します。
	 * @param includingPastHistory 過去履歴を含む
	 */
	public void setIncludingPastHistory(Boolean includingPastHistory) {
		this.includingPastHistory = includingPastHistory;
	}

	/**
	 * ステータス種別を取得します。
	 * @return ステータス種別
	 */
	public ContentSearchStatusTypeEnum getStatusType() {
		return statusType;
	}

	/**
	 * ステータス種別を設定します。
	 * @param statusType ステータス種別
	 */
	public void setStatusType(ContentSearchStatusTypeEnum statusType) {
		this.statusType = statusType;
	}

	/**
	 * テーブルIDを取得します。
	 * @return テーブルID
	 */
	public long getTableId() {
		return tableId;
	}

	/**
	 * テーブルIDを設定します。
	 * @param tableId テーブルID
	 */
	public void setTableId(long tableId) {
		this.tableId = tableId;
	}

	/**
	 * テーブル定義名を取得します。
	 * @return テーブル定義名
	 */
	public String getTableDefName() {
		return tableDefName;
	}

	/**
	 * テーブル定義名を設定します。
	 * @param tableDefName テーブル定義名
	 */
	public void setTableDefName(String tableDefName) {
		this.tableDefName = tableDefName;
	}

	/**
	 * 部分一致を取得します。
	 * @return 部分一致
	 */
	public Boolean getPartialMatch() {
		return partialMatch;
	}

	/**
	 * 部分一致を設定します。
	 * @param partialMatch 部分一致
	 */
	public void setPartialMatch(Boolean partialMatch) {
		this.partialMatch = partialMatch;
	}

	/**
	 * 属性値検索条件リストを取得します。
	 * @return 属性値検索条件リスト
	 */
	public List<ContentSearchAttributeCriteria> getAttributeCriteriaList() {
		return attributeCriteriaList;
	}

	/**
	 * 属性値検索条件リストを設定します。
	 * @param attributeCriteriaList 属性値検索条件リスト
	 */
	public void setAttributeCriteriaList(List<ContentSearchAttributeCriteria> attributeCriteriaList) {
		this.attributeCriteriaList = attributeCriteriaList;
	}

	/**
	 * ファセット絞込み条件を取得します。
	 * @return ファセット絞込み条件
	 * @since Ver6.46
	 */
	public ContentSearchFieldCriteria getFacetFieldCriteria() {
		return facetFieldCriteria;
	}

	/**
	 * ファセット絞込み条件を設定します。
	 * @param facetField ファセット絞込み条件
	 * @since Ver6.46
	 */
	public void setFacetFieldCriteria(ContentSearchFieldCriteria facetFieldCriteria) {
		this.facetFieldCriteria = facetFieldCriteria;
	}

	/**
	 * 表示タイプを取得します。
	 * @return 表示タイプ
	 */
	public DisplayTypeEnum getDisplayType() {
		return displayType;
	}

	/**
	 * 表示タイプを設定します。
	 * @param displayType 表示タイプ
	 */
	public void setDisplayType(DisplayTypeEnum displayType) {
		this.displayType = displayType;
	}

	// ---------------
	// Publicメソッド
	// ---------------

	/**
	 * 属性毎の検索条件を取得します。
	 * @param attributeTypeName 属性タイプ名
	 * @return 属性毎の検索条件
	 */
	public ContentSearchAttributeCriteria getAttributeCriteria(String attributeTypeName) {
		if (attributeCriteriaList == null) {
			return null;
		}

		ContentSearchAttributeCriteria attributeCriteria = attributeCriteriaList.stream().
			filter(attr -> attr.getAttributeType().getDefinitionName().equals(attributeTypeName)).findFirst().orElse(null);
		return attributeCriteria;
	}

	/**
	 * 属性毎の単一値検索条件を取得します。
	 * @param attributeTypeName 属性タイプ名
	 * @return 属性毎の検索条件
	 */
	public ContentSearchAttributeValueCriteria getAttributeValueCriteria(String attributeTypeName) {
		ContentSearchAttributeCriteria criteria = getAttributeCriteria(attributeTypeName);

		if (criteria instanceof ContentSearchAttributeValueCriteria) {
			return (ContentSearchAttributeValueCriteria) criteria;
		}

		return null;
	}

	/**
	 * 属性毎の単一値検索条件リストを取得します。
	 * @return 属性毎の検索条件リスト
	 */
	public List<ContentSearchAttributeValueCriteria> getAttributeValueCriteriaList() {
		List<ContentSearchAttributeValueCriteria> attributeCriteriaList = new ArrayList<>();
		for (ContentSearchAttributeCriteria attributeCriteria : this.attributeCriteriaList) {
			if (attributeCriteria instanceof ContentSearchAttributeValueCriteria) {
				attributeCriteriaList.add((ContentSearchAttributeValueCriteria)attributeCriteria);
			}
		}
		return attributeCriteriaList;
	}

	/**
	 * 属性毎の範囲指定検索条件リストを取得します。
	 * @return 属性毎の範囲指定検索条件リスト
	 */
	public List<ContentSearchAttributeRangeCriteria<?>> getAttributeRangeCriteriaList() {
		List<ContentSearchAttributeRangeCriteria<?>> attributeCriteriaList = new ArrayList<>();
		for (ContentSearchAttributeCriteria attributeCriteria : this.attributeCriteriaList) {
			if (attributeCriteria instanceof ContentSearchAttributeRangeCriteria) {
				attributeCriteriaList.add((ContentSearchAttributeRangeCriteria<?>)attributeCriteria);
			}
		}
		return attributeCriteriaList;
	}

	/**
	 * 属性毎の値なし指定検索条件リストを取得します。
	 * @return 属性毎の値なし指定検索条件リスト
	 */
	public List<ContentSearchAttributeIsNullCriteria> getAttributeIsNullCriteriaList() {
		List<ContentSearchAttributeIsNullCriteria> attributeCriteriaList = new ArrayList<>();
		for (ContentSearchAttributeCriteria attributeCriteria : this.attributeCriteriaList) {
			if (attributeCriteria instanceof ContentSearchAttributeIsNullCriteria) {
				attributeCriteriaList.add((ContentSearchAttributeIsNullCriteria)attributeCriteria);
			}
		}
		return attributeCriteriaList;
	}

	/**
	 * 属性に対する検索条件を追加します。
	 * @param attributeCriteria 属性に対する検索条件
	 */
	private void addAttributeCriteria(ContentSearchAttributeCriteria attributeCriteria) {
		if (attributeCriteriaList == null) {
			attributeCriteriaList = new ArrayList<>();
		}
		attributeCriteriaList.add(attributeCriteria);
	}

	/**
	 * 文字列比較属性値検索条件を追加します。
	 * @param attributeType 属性タイプ
	 * @param value 条件値
	 */
	public void addAttributeValueCriteria(AttributeTypeDomain attributeType, String value) {

		if(value == null || value.length() == 0) {
			return;
		}

		ContentSearchAttributeValueCriteria valueAttribute = new ContentSearchAttributeValueCriteria();
		addAttributeCriteria(valueAttribute);
		valueAttribute.setValue(value);
		valueAttribute.setAttributeType(attributeType);

	}

	/**
	 * 日時範囲指定属性値検索条件を追加します。
	 * @param attributeType 属性タイプ
	 * @param from 下限
	 * @param to 上限
	 */
	public void addAttributeRangeDateCriteria(AttributeTypeDomain attributeType, Date from, Date to) {

		if(from == null && to == null) {
			return;
		}

		ContentSearchAttributeRangeCriteria<Date> dataRangeAttribute = new ContentSearchAttributeRangeCriteria<>();
		addAttributeCriteria(dataRangeAttribute);
		RangeCriteria<Date> dateRange = new RangeCriteria<Date>(from, to);
		dataRangeAttribute.setRange(dateRange);
		dataRangeAttribute.setAttributeType(attributeType);

	}

	/**
	 * 数値範囲指定属性値検索条件を追加します。
	 * @param attributeType 属性タイプ
	 * @param from 下限
	 * @param to 上限
	 */
	public void addAttributeRangeNumericCriteria(AttributeTypeDomain attributeType, Number from, Number to) {

		if(from == null && to == null) {
			return;
		}

		ContentSearchAttributeRangeCriteria<Number> numericRangeAttribute = new ContentSearchAttributeRangeCriteria<>();
		addAttributeCriteria(numericRangeAttribute);
		RangeCriteria<Number> numericRange = new RangeCriteria<Number>(from, to);
		numericRangeAttribute.setRange(numericRange);
		numericRangeAttribute.setAttributeType(attributeType);

	}

	/**
	 * 「値なし」比較属性値検索条件を追加します。
	 * @param attributeType 属性タイプ
	 */
	public void addAttributeIsNullCriteria(AttributeTypeDomain attributeType) {

		ContentSearchAttributeIsNullCriteria isNullAttribute = new ContentSearchAttributeIsNullCriteria();
		addAttributeCriteria(isNullAttribute);
		isNullAttribute.setAttributeType(attributeType);

	}

	/**
	 * 単一値ファセット絞込み条件を取得します。
	 * @return ファセット絞込み条件
	 * @since Ver6.46
	 */
	public ContentSearchFieldValueCriteria getFieldValueCriteria() {
		if (facetFieldCriteria instanceof ContentSearchFieldValueCriteria) {
			return (ContentSearchFieldValueCriteria) facetFieldCriteria;
		}
		return null;
	}

	/**
	 * 範囲指定ファセット絞込み条件を取得します。
	 * @return ファセット絞込み条件
	 * @since Ver6.46
	 */
	public ContentSearchFieldRangeCriteria<?> getFieldRangeCriteria() {
		if (facetFieldCriteria instanceof ContentSearchFieldRangeCriteria) {
			return (ContentSearchFieldRangeCriteria<?>) facetFieldCriteria;
		}
		return null;
	}

	/**
	 * 値比較ファセットフィールド検索条件を追加します。
	 * @param fieldName フィールド名
	 * @param value 条件値
	 */
	public void addFacetFieldValueCriteria(String fieldName, String value) throws Exception {

		ContentSearchFieldValueCriteria valueCriteria = null;

		if (facetFieldCriteria == null) {
			// ファセットによる絞込み条件インタンスを生成
			valueCriteria = new ContentSearchFieldValueCriteria();
			facetFieldCriteria = valueCriteria;
			valueCriteria.setValueList(new ArrayList<>());
		} else if (facetFieldCriteria instanceof ContentSearchFieldValueCriteria) {
			// 設定済みのファセットによる絞込み条件を取得
			valueCriteria = (ContentSearchFieldValueCriteria) getFacetFieldCriteria();
		} else {
			throw new EIMApplicationException("EIM.ERROR.LOGIC.ONLY.ONE.FACET.FIELD");
		}

		// 条件を設定
		valueCriteria.setFieldName(fieldName);
		valueCriteria.getValueList().add(value);

	}

	/**
	 * 日時範囲指定ファセットフィールド検索条件を追加します。
	 * @param fieldName フィールド名
	 * @param from 下限
	 * @param to 上限
	 */
	public void addFacetFieldRangeDateCriteria(String fieldName, Date from, Date to) throws Exception {

		ContentSearchFieldRangeCriteria<Date> dateRangeCrteria = null;

		if (facetFieldRangeDateCriteria == null) {
			// ファセットによる絞込み条件インタンスを生成
			dateRangeCrteria = new ContentSearchFieldRangeCriteria<>();
			facetFieldRangeDateCriteria = dateRangeCrteria;
			facetFieldCriteria = dateRangeCrteria;
			dateRangeCrteria.setRangeList(new ArrayList<>());
		} else if (facetFieldCriteria == facetFieldRangeDateCriteria) {
			// 設定済みのファセットによる絞込み条件を取得
			dateRangeCrteria = facetFieldRangeDateCriteria;
		} else {
			throw new EIMApplicationException("EIM.ERROR.LOGIC.ONLY.ONE.FACET.FIELD");
		}

		// 条件を設定
		dateRangeCrteria.setFieldName(fieldName);
		RangeCriteria<Date> dateRange = new RangeCriteria<Date>(from, to);
		dateRangeCrteria.getRangeList().add(dateRange);

	}

	/**
	 * 数値範囲指定ファセットフィールド検索条件を追加します。
	 * @param fieldName フィールド名
	 * @param from 下限
	 * @param to 上限
	 */
	public void addFacetFieldRangeNumericCriteria(String fieldName, Number from, Number to) throws Exception {

		// 数値範囲指定の場合は文字列として扱う
		ContentSearchFieldRangeCriteria<Number> numericRangeCrteria = null;

		if (facetFieldCriteria == null) {
			// ファセットによる絞込み条件インタンスを生成
			numericRangeCrteria = new ContentSearchFieldRangeCriteria<>();
			facetFieldCriteria = numericRangeCrteria;
			numericRangeCrteria.setRangeList(new ArrayList<>());
		} else if (facetFieldCriteria == facetFieldRangeNumericCriteria) {
			// 設定済みのファセットによる絞込み条件を取得
			numericRangeCrteria = facetFieldRangeNumericCriteria;
		} else {
			throw new EIMApplicationException("EIM.ERROR.LOGIC.ONLY.ONE.FACET.FIELD");
		}

		// 条件を設定
		numericRangeCrteria.setFieldName(fieldName);
		RangeCriteria<Number> numberRange = new RangeCriteria<Number>(from, to);
		numericRangeCrteria.getRangeList().add(numberRange);

	}

}
