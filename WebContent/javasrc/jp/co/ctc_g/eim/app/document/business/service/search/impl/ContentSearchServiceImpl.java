package jp.co.ctc_g.eim.app.document.business.service.search.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.util.AppConstant;
import common.util.CustomDefaultTableUtils;
import eim.bo.EIMAttributeType;
import eim.bo.EIMTable;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.TableUtils;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchAttributeIsNullCriteria;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchAttributeRangeCriteria;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchAttributeValueCriteria;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchCriteria;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchCriteria.ContentSearchTypeEnum;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchFieldRangeCriteria;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchFieldValueCriteria;
import jp.co.ctc_g.eim.app.document.business.service.search.ContentSearchService;
import jp.co.ctc_g.eim.app.document.common.enumeration.search.ContentSearchFieldEnum;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.AttributeTypeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.RangeCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.service.AttributeTypeService;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;
import jp.co.ctc_g.eim.search.core.common.enumeration.FlagValueEnum;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchRecordDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchResultsDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.ValueFieldDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.criteria.KeywordCriteria;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.criteria.SearchConditionGroup.LogicalOperator;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.criteria.SearchConditionRange.BoundaryTypeEnum;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.criteria.SearchCriteria;
import jp.co.ctc_g.eim.search.core.searchApi.business.service.SearchService;
import jp.co.ctc_g.eim.search.solr.common.enumeration.DynamicFieldTypeEnum;
import jp.co.ctc_g.eim.search.solr.searchApi.business.domain.criteria.SolrSearchCriteria;
import jp.co.ctc_g.eim.search.solr.searchApi.business.service.impl.SolrSearchServiceImpl;

/**
 * Solrに登録されているコンテンツ(ドキュメント、フォルダ、タグ)を条件に従い検索取得するためのサービスクラスです。
 *
 */
public class ContentSearchServiceImpl implements ContentSearchService {

	/** Logger */
	private static final Log log = LogFactory.getLog(ContentSearchServiceImpl.class);

	/** 取得件数(本文抜粋表示) */
	private Integer resultRowsTextExcerpt = null;

	/** 取得件数(リスト表示) */
	private Integer resultRowsList = null;

	/** 取得件数(リスト表示) */
	private Integer resultRowsThumbnail = null;

	/** 検索サービス */
	SearchService searchService = new SolrSearchServiceImpl();

	/** 属性タイプサービス */
	private AttributeTypeService attributeTypeService = null;

	/** アンエスケープ対象のワイルドカード文字リスト */
	private static final String[] wildCards = {"*", "?"};

	// ----------------------
	// Publicメソッド
	// ----------------------

	@Override
	public SearchResultsDomain search(ContentSearchCriteria criteria) throws Exception {

		long start = System.currentTimeMillis();

		// ----------------------
		// 検索条件
		// ----------------------

		// 検索条件クライテリアを生成
		SearchCriteria searchCriteria = convertToSearchCriteria(criteria);

		// キーワード条件クライテリアを生成
		KeywordCriteria keywordCriteria = new KeywordCriteria();

		// キーワードを条件に追加
		keywordCriteria.setKeyword(criteria.getKeyword());
		keywordCriteria.setIncludingFullText(criteria.getIncludingFullText());

		// -----------------
		// 取得条件
		// -----------------

		switch (criteria.getDisplayType()) {
		case TEXT_EXCERPT:
			// スニペットを取得する
			keywordCriteria.setContainSnippets(true);

			// 取得件数
			searchCriteria.setResultRows(resultRowsTextExcerpt);
			break;

		case LIST:
			// スニペットを取得しない
			keywordCriteria.setContainSnippets(false);

			// 取得件数
			searchCriteria.setResultRows(resultRowsList);
			break;

		case THUMBNAIL:
			// スニペットを取得しない
			keywordCriteria.setContainSnippets(false);
			// 取得件数
			searchCriteria.setResultRows(resultRowsThumbnail);
			break;
		}

		// 取得フィールド
		long tableId = criteria.getTableId();
		List<String> fieldNameList = getFieldListByTableId(tableId,criteria.getTableDefName());
		searchCriteria.setResultFieldNameList(fieldNameList);

		// 他の取得条件はSearchServiceのプロパティで設定する

		// ----------------------
		// 検索実行
		// ----------------------

		SearchResultsDomain results = searchService.search(searchCriteria, keywordCriteria);

		// 検索結果が0件、ドキュメント検索以外、キーワードの指定なし、全文を含まない場合はそのまま返却
		if (results.getRecordList().size() == 0 ||
				criteria.getSearchType() != ContentSearchTypeEnum.DOCUMENT ||
				criteria.getKeyword() == null ||
				criteria.getKeyword().length() == 0 ||
				!criteria.getIncludingFullText()) {
			return results;
		}

		// --------------------------------------
		// ページ検索 (ページにヒットするか検査)
		// --------------------------------------

		// 検索結果レコードからKEY項目を取得
		Map<String, List<SearchRecordDomain>> keyRecordMap = new HashMap<>();
		for (SearchRecordDomain record : results.getRecordList()) {
			ValueFieldDomain<String> keyField = record.getValueField(ContentSearchFieldEnum.KEY.toString());
			ValueFieldDomain<Integer> seqField = record.getValueField(ContentSearchFieldEnum.SEQ.toString());

			// KEYの編集 (EIMOBJスキーマのKEYに添え字が付与されている場合は除去)
			String parentKey = null;
			if (seqField.getValue() > 0) {
				parentKey = keyField.getValue().replaceAll("_" + seqField.getValue() + "$", "");
			} else {
				parentKey = keyField.getValue();
			}

			// 編集後のKEYとレコードListをMapに保管
			if (!keyRecordMap.containsKey(parentKey)) {
				keyRecordMap.put(parentKey, new ArrayList<>());
			}
			keyRecordMap.get(parentKey).add(record);
		}

		// ページ検索実行
		List<String> parentList = searchService.searchPagesParents(criteria.getKeyword(), new ArrayList<>(keyRecordMap.keySet()));

		// ページ検索実行結果とキーが一致するレコードにページフラグtrueを設定
		for (String parentKey : parentList) {
			keyRecordMap.get(parentKey).stream()
				.forEach(record -> record.getValueFieldList()
					.add(new ValueFieldDomain<Integer>(ContentSearchFieldEnum.MATCH_PAGES.toString(), FlagValueEnum.ON.getValue())));
		}

		long end = System.currentTimeMillis();
		if (log.isTraceEnabled()) {
			log.trace("経過時間1=" + (end - start) + "ms");
		}

		return results;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.search.ContentSearchService#suggest(jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchCriteria)
	 */
	@Override
	public List<String> suggest(String letters, ContentSearchCriteria criteria) throws Exception {

		long start = System.currentTimeMillis();

		// ----------------------
		// 検索条件
		// ----------------------

		// 検索条件クライテリアを生成
		SearchCriteria searchCriteria = convertToSearchCriteria(criteria);

		// ----------------------
		// 検索実行
		// ----------------------

		List<String> results = searchService.suggest(letters, searchCriteria);

		long end = System.currentTimeMillis();
		if (log.isTraceEnabled()) {
			log.trace("経過時間1=" + (end - start) + "ms");
		}

		return results;
	}

	/**
	 * @see jp.co.ctc_g.eim.app.document.business.service.search.ContentSearchService#searchPages(java.lang.String, long)
	 */
	@Override
	public List<Integer> searchPages(String keyword, long objId) throws Exception {

		long start = System.currentTimeMillis();

		// ----------------------
		// 検索条件
		// ----------------------
		SearchCriteria parentCriteria = new SolrSearchCriteria();
		parentCriteria.addConditionString(ContentSearchFieldEnum.ID.toString(), String.valueOf(objId));

		// ----------------------
		// 検索実行
		// ----------------------

		List<Integer> results = searchService.searchPages(keyword, parentCriteria);

		long end = System.currentTimeMillis();
		if (log.isTraceEnabled()) {
			log.trace("経過時間1=" + (end - start) + "ms");
		}

		return results;
	}

	// ----------------------
	// Privateメソッド
	// ----------------------

	/**
	 * 検索条件クライテリアを生成して返却します。<br>
	 * search()、suggest()で共用するためにキーワードの設定は行いません。
	 * @param criteria
	 * @return
	 * @throws Exception
	 */
	private SearchCriteria convertToSearchCriteria(ContentSearchCriteria criteria) throws Exception {

		// ----------------------
		// 共通検索条件生成
		// ----------------------

		SearchCriteria searchCriteria = new SolrSearchCriteria();

		// 検索種別
		ContentSearchTypeEnum contentType = criteria.getSearchType();
		searchCriteria.addConditionString(ContentSearchFieldEnum.DATA_TYPE.toString(), contentType.toString());

		// 検索対象
		String searchPath = criteria.getSearchPath();
		if (searchPath != null && searchPath.length() > 0) {
			searchCriteria.addConditionString(ContentSearchFieldEnum.PATH.toString(), searchPath + "*");
		}

		// 過去履歴を含む (Offの場合) (ドキュメントの場合有効)
		if (contentType == ContentSearchTypeEnum.DOCUMENT &&
				(criteria.getIncludingPastHistory() == null || criteria.getIncludingPastHistory() == false)) {
			// LATEST = 1 or SEQ >= 1(リンクの場合)
			searchCriteria
				.push(LogicalOperator.OR)
					.addConditionNumeric(ContentSearchFieldEnum.LATEST.toString(), FlagValueEnum.ON.getValue())
					.addConditionRangeNumeric(ContentSearchFieldEnum.SEQ.toString(), 1, null)
				.pop();
		}

		// 空フォルダ
		if (criteria.getEmptyFolder() != null && criteria.getEmptyFolder() == true) {
			searchCriteria.addConditionNumeric(ContentSearchFieldEnum.EMPTY_FOLDER.toString(), FlagValueEnum.ON.getValue());
		}

		// ステータス(ドキュメント/フォルダの場合有効)
		if (contentType != ContentSearchTypeEnum.TAG && criteria.getStatusType() != null) {
			switch (criteria.getStatusType()) {
			case ALL:
				break;
			case CHECKOUT:
				// 改訂中
				searchCriteria.push(LogicalOperator.OR)
					// ステータスなし公開
					.push()
						.addConditionIsNull(ContentSearchFieldEnum.STATUSTYPE_KIND.toString())
						.addConditionNumeric(ContentSearchFieldEnum.PUBLIC.toString(), FlagValueEnum.ON.getValue())
					.pop()
					// 公開済ステータス
					.addConditionNumeric(ContentSearchFieldEnum.STATUSTYPE_KIND.toString(), AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
				.pop()
				// ロック中
				.addConditionIsNull(ContentSearchFieldEnum.LOCK_DATE.toString(), true);
				break;
			case EDIT:
				// 未公開
				searchCriteria.push(LogicalOperator.OR)
					// ステータスなし非公開
					.push()
						.addConditionIsNull(ContentSearchFieldEnum.STATUSTYPE_KIND.toString())
						.addConditionNumeric(ContentSearchFieldEnum.PUBLIC.toString(), FlagValueEnum.OFF.getValue())
					.pop()
					// 公開済ステータス以外
					.push()
						.addConditionIsNull(ContentSearchFieldEnum.STATUSTYPE_KIND.toString(), true)
						.addConditionNumeric(ContentSearchFieldEnum.STATUSTYPE_KIND.toString(), AppConstant.STATUS_TYPE_KIND_ID_PUBLIC, true)
					.pop()
				.pop();
				break;
			case PUBLIC:
				// 公開済み
				searchCriteria.push(LogicalOperator.OR)
					// ステータスなし公開
					.push()
						.addConditionIsNull(ContentSearchFieldEnum.STATUSTYPE_KIND.toString())
						.addConditionNumeric(ContentSearchFieldEnum.PUBLIC.toString(), FlagValueEnum.ON.getValue())
					.pop()
					// 公開済ステータス
					.addConditionNumeric(ContentSearchFieldEnum.STATUSTYPE_KIND.toString(), AppConstant.STATUS_TYPE_KIND_ID_PUBLIC)
				.pop();
				break;
			}
		}

		// ----------------------
		// 属性値詳細検索条件生成
		// ----------------------

		// 部分一致
		boolean partialMatch = criteria.getPartialMatch();

		if (criteria.getAttributeCriteriaList() != null && criteria.getAttributeCriteriaList().size() > 0) {

			// 拡張属性用に属性タイプを取得
			List<Long> attributeTypeIdList =
					criteria.getAttributeCriteriaList().stream().
					map(item -> item.getAttributeType().getId()).filter(id -> id > 0).collect(Collectors.toList());
			AttributeTypeCriteria attributeTypeCriteria = new AttributeTypeCriteria();
			attributeTypeCriteria.setIds(attributeTypeIdList);
			List<AttributeTypeDomain> userAttributeTypeList = attributeTypeService.getList(attributeTypeCriteria);
			Map<Long, AttributeTypeDomain> attributeTypeMap =
					userAttributeTypeList.stream().collect(Collectors.toMap(AttributeTypeDomain::getId, UnaryOperator.identity()));

			// 単一値指定属性
			if (criteria.getAttributeValueCriteriaList() != null && criteria.getAttributeValueCriteriaList().size() > 0) {
				// 部分一致無効フィールド
				List<String> invalidPartialMatchFieldList = Arrays.asList(
						ContentSearchFieldEnum.ID.toString(),
						ContentSearchFieldEnum.AUTO_NUMBER.toString());
				for (ContentSearchAttributeValueCriteria valueAttributeCriteria : criteria.getAttributeValueCriteriaList()) {
					// 拡張属性の場合は属性タイプを取得したものに置き替える
					if (valueAttributeCriteria.getAttributeType().getId() > 0) {
						valueAttributeCriteria.setAttributeType(attributeTypeMap.get(valueAttributeCriteria.getAttributeType().getId()));
					}
					String fieldName = convertToFieldName(valueAttributeCriteria.getAttributeType());
					String value = valueAttributeCriteria.getValue();
					// 部分一致が指定されていて部分一致が有効なフィールドの場合
					if (partialMatch && !invalidPartialMatchFieldList.contains(fieldName)) {
						value = "*" + value + "*";
					}
					searchCriteria.addConditionString(fieldName, value);
				}
			}

			// 範囲指定属性
			if (criteria.getAttributeRangeCriteriaList() != null && criteria.getAttributeRangeCriteriaList().size() > 0) {
				for (ContentSearchAttributeRangeCriteria<?> rangeAttributeCriteria : criteria.getAttributeRangeCriteriaList()) {
					// 拡張属性の場合は属性タイプを取得したものに置き替える
					if (rangeAttributeCriteria.getAttributeType().getId() > 0) {
						rangeAttributeCriteria.setAttributeType(attributeTypeMap.get(rangeAttributeCriteria.getAttributeType().getId()));
					}
					String fieldName = convertToFieldName(rangeAttributeCriteria.getAttributeType());
					RangeCriteria<?> range = rangeAttributeCriteria.getRange();
					if (range.getFrom() instanceof Date || range.getTo() instanceof Date) {
						// 日時型の場合
						searchCriteria.addConditionRangeDate(fieldName, (Date) range.getFrom(), (Date) range.getTo());
					} else {
						// 数値型の場合 (検索条件文字列として設定する)
						searchCriteria.addConditionRangeNumeric(fieldName, (Number) range.getFrom(), (Number) range.getTo());
					}
				}
			}

			// 値なし指定属性
			if (criteria.getAttributeIsNullCriteriaList() != null && criteria.getAttributeIsNullCriteriaList().size() > 0) {
				for (ContentSearchAttributeIsNullCriteria isNullAttributeCriteria : criteria.getAttributeIsNullCriteriaList()) {
					// 拡張属性の場合は属性タイプを取得したものに置き替える
					if (isNullAttributeCriteria.getAttributeType().getId() > 0) {
						isNullAttributeCriteria.setAttributeType(attributeTypeMap.get(isNullAttributeCriteria.getAttributeType().getId()));
					}
					String fieldName = convertToFieldName(isNullAttributeCriteria.getAttributeType());
					searchCriteria.addConditionIsNull(fieldName);
				}
			}
		}

		// ----------------------
		// ファセット検索条件生成
		// ----------------------

		// レンジファセット検索条件式
		if (criteria.getFieldRangeCriteria() != null) {
			searchCriteria.push(LogicalOperator.OR);
			ContentSearchFieldRangeCriteria<?> rangeFieldCriteria = criteria.getFieldRangeCriteria();
			for (RangeCriteria<?> facet : rangeFieldCriteria.getRangeList()) {
				if (facet.getFrom() instanceof Date || facet.getTo() instanceof Date) {
					// 日時型の場合
					searchCriteria.addConditionRangeDate(
							rangeFieldCriteria.getFieldName(), (Date) facet.getFrom(), (Date) facet.getTo(), BoundaryTypeEnum.EXCLUDE_TO);
				} else {
					// 数値型の場合 (検索条件文字列として設定する)
					searchCriteria.addConditionRangeNumeric(
							rangeFieldCriteria.getFieldName(), (Number) facet.getFrom(), (Number) facet.getTo(), BoundaryTypeEnum.EXCLUDE_TO);
				}
			}
			searchCriteria.pop();
		}

		// 単一値ファセット検索条件式
		if (criteria.getFieldValueCriteria() != null) {
			searchCriteria.push(LogicalOperator.OR);
			ContentSearchFieldValueCriteria fieldCriteria = criteria.getFieldValueCriteria();
			for (String facet : fieldCriteria.getValueList()) {
				searchCriteria.addConditionString(fieldCriteria.getFieldName(), escapeWildCards(facet));
			}
			searchCriteria.pop();
		}

		return searchCriteria;
	}

	/**
	 * ワイルドカード文字列のエスケープ処理を行います。
	 * @param str エスケープする文字列
	 * @return エスケープされた文字列
	 */
	private String escapeWildCards(String str) {
		String escapedStr = str;

		// ワイルドカードのエスケープ処理
		for (String c : wildCards) {
			escapedStr = escapedStr.replace(c, "\\" + c);
		}

		return escapedStr;
	}

	/**
	 * 表示項目テーブルIDから取得対象フィールドのリストを取得します。
	 * @param tableId テーブルID
	 * @param tableId テーブル定義名
	 * @return 取得対象フィールドのリスト
	 * @throws Exception
	 */
	private List<String> getFieldListByTableId(long tableId, String tableDefName) throws Exception {

		// セッション情報取得
		EIMSession siteSess = EIMThreadContext.getEIMSession();

		// 常時取得フィールド
		List<String> alwaysFieldList = Arrays.asList(
				ContentSearchFieldEnum.ID.toString(),
				ContentSearchFieldEnum.DATA_TYPE.toString(),
				ContentSearchFieldEnum.OBJECT_NAME.toString(),
				ContentSearchFieldEnum.LOCK_DATE.toString(),
				ContentSearchFieldEnum.LOCK_USER_LANG_ID_NAME.toString(),
				ContentSearchFieldEnum.STATUSTYPE_KIND.toString(),
				ContentSearchFieldEnum.STATUSTYPE_LANG_ID_NAME.toString(),
				ContentSearchFieldEnum.REVISION.toString(),
				ContentSearchFieldEnum.LATEST.toString(),
				ContentSearchFieldEnum.PUBLIC_PROC_FAILURE.toString(),
				ContentSearchFieldEnum.PATH.toString(),
				ContentSearchFieldEnum.PUBLIC.toString(),
				ContentSearchFieldEnum.WF_ATTACHED_FOLDER_ID.toString(),
				ContentSearchFieldEnum.EXTENSION.toString(),
				ContentSearchFieldEnum.SEQ.toString(),
				ContentSearchFieldEnum.LINK_PARENT_ID.toString(),
				ContentSearchFieldEnum.LINK_UPDATE_TIMING.toString(),
				ContentSearchFieldEnum.PDF_CONV_EXEC_DATE.toString(),
				ContentSearchFieldEnum.PDF_PRE_REGIST_DATE.toString(),
				ContentSearchFieldEnum.FAILED_TO_PDF_MERGE.toString(),
				ContentSearchFieldEnum.OCR_PROC_STATUS.toString(),
				ContentSearchFieldEnum.OCR_RESULT_STATUS.toString(),
				ContentSearchFieldEnum.MODIFY_DATE.toString(),
				ContentSearchFieldEnum.MODIFY_USER_LANG_ID_NAME.toString(),
				ContentSearchFieldEnum.EFFECTIVE_TERM.toString()
		);

		List<String> fieldList = new ArrayList<>(alwaysFieldList);
		if (tableId == 0 && tableDefName == null) {
			// システムデフォルト選択時
			fieldList.addAll(Arrays.asList(
					ContentSearchFieldEnum.PROPERTY.toString()
			));
		} else {
			List<EIMAttributeType> attTypeList = new ArrayList<EIMAttributeType>();
			if (tableId != 0) {
				// ユーザ個別設定テーブル選択時
				// テーブル情報から属性タイプを取得
				EIMTable table = TableUtils.getTableById(siteSess, tableId);
				@SuppressWarnings("unchecked")
				List<EIMAttributeType> attributeList = table.getAttributeList();
				attTypeList = attributeList;
			} else {
				// カスタムデフォルトテーブル選択時
				attTypeList = CustomDefaultTableUtils.getSelectedTableAttributeTypeListByDefName(siteSess, tableDefName);
			}

			for (EIMAttributeType attType : attTypeList) {
				// EIMAttributeをAttributeTypeDomainに変換
				AttributeTypeDomain attTypeDomain = new AttributeTypeDomain(attType.getId());
				attTypeDomain.setDefinitionName(attType.getDefName());
				attTypeDomain.setMultiple(attType.isMultiple());
				switch (attType.getValueType().getId()) {
				case EIMValueType.DATE:
					attTypeDomain.setValueType(ValueTypeEnum.DATE);
					break;
				case EIMValueType.DOUBLE:
					attTypeDomain.setValueType(ValueTypeEnum.DOUBLE);
					break;
				case EIMValueType.INTEGER:
					attTypeDomain.setValueType(ValueTypeEnum.LONG);
					break;
				case EIMValueType.STRING:
					attTypeDomain.setValueType(ValueTypeEnum.STRING);
					break;
				case EIMValueType.TEXT:
					attTypeDomain.setValueType(ValueTypeEnum.TEXT);
					break;
				case EIMValueType.CODE:
				case EIMValueType.OBJECT:
				case EIMValueType.USER:
					// サポート外
					continue;
				}

				// フィールド名を取得して取得対象リストに追加
				String fieldName = convertToFieldName(attTypeDomain);
				fieldList.add(fieldName);
			}
		}

		// 自動採番が有効な場合、番号フィールドを追加
		String automaticNumbering = EIMConfig.getValue("ENABLE_AUTOMATIC_NUMBERING");
		if(automaticNumbering != null && automaticNumbering.toUpperCase().equals("ON")){
			fieldList.add(ContentSearchFieldEnum.AUTO_NUMBER.toString());
		}

		return fieldList;

	}

	/**
	 * 検索エンジンのフィールド名称を取得します。
	 * @param attributeType
	 * @return
	 */
	private String convertToFieldName(AttributeTypeDomain attributeType) {

		// ----------------------
		// 静的フィールドの場合
		// ----------------------

		ContentSearchFieldEnum fieldEnum = ContentSearchFieldEnum.getByAttribuetTypeName(attributeType.getDefinitionName());
		if (fieldEnum != null) {
			// フィールド名を返却
			return fieldEnum.toString();
		}

		// ----------------------
		// 動的フィールドの場合
		// ----------------------

		// データ種別毎にダイナミックフィールド列挙型を取得
		DynamicFieldTypeEnum fieldType = null;
		switch (attributeType.getValueType()) {
		case DATE:
			fieldType =  attributeType.isMultiple() == false ? DynamicFieldTypeEnum.DATE : DynamicFieldTypeEnum.DATES;
			break;
		case DOUBLE:
			fieldType =  attributeType.isMultiple() == false ? DynamicFieldTypeEnum.DOUBLE : DynamicFieldTypeEnum.DOUBLES;
			break;
		case LONG:
			fieldType =  attributeType.isMultiple() == false ? DynamicFieldTypeEnum.LONG : DynamicFieldTypeEnum.LONGS;
			break;
		case STRING:
			fieldType =  attributeType.isMultiple() == false ? DynamicFieldTypeEnum.STRING : DynamicFieldTypeEnum.STRINGS;
			break;
		case TEXT:
			fieldType =  attributeType.isMultiple() == false ? DynamicFieldTypeEnum.TEXT : DynamicFieldTypeEnum.TEXTS;
			break;
		// 以下は未対応
		case CODE:
		case OBJECT:
		case USER:
			return null;
		}

		// フィールド名を返却
		return fieldType.getFieldName(attributeType.getId());
	}

	// ----------------------
	// getter/setter
	// ----------------------

	/**
	 * 取得件数(本文抜粋表示)を取得します。
	 * @return 取得件数(本文抜粋表示)
	 */
	public Integer getResultRowsTextExcerpt() {
		return resultRowsTextExcerpt;
	}

	/**
	 * 取得件数(本文抜粋表示)を設定します。
	 * @param resultRowsTextExcerpt 取得件数(本文抜粋表示)
	 */
	public void setResultRowsTextExcerpt(Integer resultRowsTextExcerpt) {
		this.resultRowsTextExcerpt = resultRowsTextExcerpt;
	}

	/**
	 * 取得件数(リスト表示)を取得します。
	 * @return 取得件数(リスト表示)
	 */
	public Integer getResultRowsList() {
		return resultRowsList;
	}

	/**
	 * 取得件数(リスト表示)を設定します。
	 * @param resultRowsList 取得件数(リスト表示)
	 */
	public void setResultRowsList(Integer resultRowsList) {
		this.resultRowsList = resultRowsList;
	}

	/**
	 * 取得件数(サムネイル表示)を取得します。
	 * @param resultRowsThumbnail 取得件数(サムネイル表示)
	 */
	public Integer getResultRowsThumbnail() {
		return resultRowsThumbnail;
	}

	/**
	 * 取得件数(サムネイル表示)を設定します。
	 * @param resultRowsThumbnail 取得件数(サムネイル表示)
	 */
	public void setResultRowsThumbnail(Integer resultRowsThumbnail) {
		this.resultRowsThumbnail = resultRowsThumbnail;
	}

	/**
	 * 検索サービスを取得します。
	 * @return 検索サービス
	 */
	public SearchService getSearchService() {
		return searchService;
	}

	/**
	 * 検索サービスを設定します。
	 * @param searchService 検索サービス
	 */
	public void setSearchService(SearchService searchService) {
		this.searchService = searchService;
	}

	/**
	 * 属性タイプサービスを取得します。
	 * @return 属性タイプサービス
	 */
	public AttributeTypeService getAttributeTypeService() {
		return attributeTypeService;
	}

	/**
	 * 属性タイプサービスを設定します。
	 * @param attributeTypeService 属性タイプサービス
	 */
	public void setAttributeTypeService(AttributeTypeService attributeTypeService) {
		this.attributeTypeService = attributeTypeService;
	}

}
