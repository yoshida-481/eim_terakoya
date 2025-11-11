/**
 *
 */
package jp.co.ctc_g.eim.app.document.presentation.controller.search;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.stereotype.Controller;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import common.util.AppConstant;
import common.util.AppLogicUtil;
import eim.net.EIMSession;
import eim.util.DateUtils;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchCriteria;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchCriteria.ContentSearchStatusTypeEnum;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchCriteria.ContentSearchTypeEnum;
import jp.co.ctc_g.eim.app.document.business.domain.search.criteria.ContentSearchCriteria.DisplayTypeEnum;
import jp.co.ctc_g.eim.app.document.business.service.PublicDocumentService;
import jp.co.ctc_g.eim.app.document.business.service.search.ContentSearchService;
import jp.co.ctc_g.eim.app.document.common.enumeration.search.ContentSearchFieldEnum;
import jp.co.ctc_g.eim.app.document.common.enumeration.search.ContentTypeEnum;
import jp.co.ctc_g.eim.app.document.presentation.dto.search.ContentSearchRecordDTO;
import jp.co.ctc_g.eim.app.document.presentation.dto.search.ContentSearchResultsDTO;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.MultipleCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.ObjectCriteria;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.common.enumeration.ValueTypeEnum;
import jp.co.ctc_g.eim.framework2.common.exception.EIMApplicationException;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.presentation.web.controller.RestController;
import jp.co.ctc_g.eim.search.core.common.enumeration.FlagValueEnum;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchRecordDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchResultsDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SnippetFieldDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.ValueFieldDomain;
import jp.co.ctc_g.eim.search.solr.common.enumeration.FileTypeEnum;

/**
 * コンテンツ(ドキュメント・フォルダ・タグ)検索実行コントローラクラスです。
 *
 */
@Controller
@RequestMapping({ "/rest/app/document/search" })
public class ContentSearchController extends RestController {

	/** 公開ドキュメントサービス */
	private PublicDocumentService publicDocumentService = null;

	/** カスタム属性パラメータ名プレフィックス */
	private final String PARAM_NAME_PREFIX_CUSTOM_ATTRIBUTE = "attType_";

	/** カスタム属性パラメータ名複数値サフィックス */
	private final String PARAM_NAME_SUFFIX_MULTI_VALUE = "_multivalue";

	/**
	 * コンテンツ検索サービス
	 */
	private ContentSearchService contentSearchService = null;

	/** オブジェクトサービス */
	private ObjectService objectDaoWithoutAttribute;

	/**
	 * コンテンツ検索サービスを設定します。
	 * @param contentSearchService オブジェクト検索サービス
	 */
	public void setContentSearchService(ContentSearchService contentSearchService) {
		this.contentSearchService = contentSearchService;
	}

	// ----------------
	// Public関数
	// ----------------

	/**
	 * コンテンツ(ドキュメント・フォルダ・タグ)検索処理を実行します。
	 * @param form
	 * @return
	 * @throws Exception
	 */
	@RequestMapping(method=RequestMethod.POST)
	@ResponseBody
	public ContentSearchResultsDTO search(@RequestBody MultiValueMap<String, String> form) throws Exception {

		// -----------------
		// パラメータ
		// -----------------

		// キーワード
		String keyword = form.getFirst("keyword");

		// ----------------
		// 検索条件
		// ----------------

		// 検索条件クライテリアを生成
		ContentSearchCriteria criteria = convertToCriteria(form);

		// キーワードを条件に追加
		if (keyword != null && keyword.length() > 0) {
			criteria.setKeyword(keyword);
		}

		// -----------------
		// 検索実行
		// -----------------

		SearchResultsDomain results = contentSearchService.search(criteria);

		// -----------------
		// 結果データ生成
		// -----------------

		// 検索結果ドメインをDTOに変換
		ContentSearchResultsDTO resultDto = convertToDto(results);

		return resultDto;

	}

	/**
	 * キーワード検索においてキーワード文字列を補完して提案します。
	 * @param form
	 * @return
	 * @throws Exception
	 */
	@RequestMapping(value = "/suggest", method=RequestMethod.POST)
	@ResponseBody
	public List<String> suggest(@RequestBody MultiValueMap<String,String> form) throws Exception {

		// -----------------
		// パラメータ
		// -----------------

		// キーワードの指定がない場合は空を返却
		String keyword = form.getFirst("keyword");
		if (keyword == null || keyword.length() == 0) {
			return new ArrayList<>();
		}

		// ----------------
		// 検索条件
		// ----------------

		// 検索条件クライテリアを生成
		ContentSearchCriteria criteria = convertToCriteria(form);

		// -----------------
		// 検索実行
		// -----------------

		List<String> resultList = contentSearchService.suggest(keyword, criteria);

		return resultList;
	}

	/**
	 * キーワード検索においてPDFファイルの該当ページ番号を特定します。
	 * @param form
	 * @return
	 * @throws Exception
	 */
	@RequestMapping(value = "/pages", method=RequestMethod.POST)
	@ResponseBody
	public List<Integer> searchPages(@RequestBody MultiValueMap<String,String> form) throws Exception {

		// -----------------
		// パラメータ
		// -----------------

		// キーワード (指定がない場合は空を返却)
		String keyword = form.getFirst("keyword");
		if (keyword == null || keyword.length() == 0) {
			return null;
		}

		// オブジェクトID
		Long objId = Long.valueOf(form.getFirst("objId"));

		// -----------------
		// 検索実行
		// -----------------

		List<Integer> results = contentSearchService.searchPages(keyword, objId);

		return results;
	}

	// ----------------
	// Private関数
	// ----------------

	/**
	 * リスクエストパラメータから検索条件クライテリアを再生して返却します。<br>
	 * search()、suggest()で共用するためにキーワードの設定は行いません。
	 * @param form
	 * @return
	 * @throws Exception
	 */
	private ContentSearchCriteria convertToCriteria(MultiValueMap<String,String> form) throws Exception {

		// -----------------
		// パラメータ
		// -----------------

		// 検索種別
		String searchType = form.getFirst("searchType");

		// 上記フォルダ以下のみを検索
		boolean pathCondition = Boolean.valueOf(form.getFirst("pathCondition"));

		// 検索対象
		String searchPath = form.getFirst("searchPath");

		// 検索対象がごみ箱かどうか
		boolean isTrash = Boolean.valueOf(form.getFirst("isTrash"));

		// 全文を含む
		boolean contents = Boolean.valueOf(form.getFirst("contents"));

		// 過去履歴を含む
		boolean history = Boolean.valueOf(form.getFirst("history"));

		// 空フォルダ
		boolean emptyFolder = Boolean.valueOf(form.getFirst("emptyFolder"));

		// ステータス
		String status = form.getFirst("status");

		// テーブルID
		String tableId = form.getFirst("tableId");

		// テーブル定義名
		String tableDefName = form.getFirst("tableDefName");

		// 部分一致
		boolean chkDetailLikeSearch = Boolean.valueOf(form.getFirst("chkDetailLikeSearch"));

		// 更新者
		String modifyUserName = form.getFirst("modifyUserName");

		// 更新日時
		String modifyDate = form.getFirst("modifyDate");

		// 作成者
		String createUserName = form.getFirst("createUserName");

		// 作成日時
		String createDate = form.getFirst("createDate");

		// プロパティ
		String property = form.getFirst("property");

		// 有効期限
		String effectiveTerm = form.getFirst("effectiveTerm");

		// 文書ID
		String searchIndex = form.getFirst("searchIndex");

		// 番号(自動採番)
		String autoNumber = form.getFirst("number");

		// 単一値拡張属性
		List<String> attType_Value = form.get("attType_Value");

		// 日時拡張属性
		List<String> attType_RangeDate = form.get("attType_RangeDate");

		// 数値(整数/実数)拡張属性
		List<String> attType_RangeNumeric = form.get("attType_RangeNumeric");

		// 単一値ファセット
		List<String> facet_Value = form.get("facet_Value");

		// 日時レンジファセット
		List<String> facet_RangeDate = form.get("facet_RangeDate");

		// 数値(整数/実数)レンジファセット
		List<String> facet_RangeNumeric = form.get("facet_RangeNumeric");

		// 表示タイプ (本文抜粋 or リスト)
		String displayTypeId = form.getFirst("displayTypeId");

		// -----------------
		// 検索条件
		// -----------------

		ContentSearchCriteria criteria = new ContentSearchCriteria();

		// 検索種別
		criteria.setSearchType(ContentSearchTypeEnum.getByValue(searchType));

		// 検索対象(パス)
		if (pathCondition) {
			String langId = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession().getLangId();
			if (isTrash && AppConstant.LANG_VALUE_EN.equals(langId)) {
				// 検索対象がごみ箱の場合、パスの「Garbage box」は「ごみ箱」に置換する
				searchPath = replaceSearchPathForEn(searchPath);
			}
			criteria.setSearchPath(searchPath);
		}

		// 空フォルダ
		criteria.setEmptyFolder(emptyFolder);

		// 全文を含む
		criteria.setIncludingFullText(contents);

		// 過去履歴を含む
		criteria.setIncludingPastHistory(history);

		// ステータス種別
		criteria.setStatusType(ContentSearchStatusTypeEnum.getByValue(status));

		// 部分一致
		criteria.setPartialMatch(chkDetailLikeSearch);

		// 更新者
		if (modifyUserName != null && modifyUserName.length() > 0) {
			AttributeTypeDomain attributeType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_MODIFY_USER"));
			attributeType.setValueType(ValueTypeEnum.STRING);
			// 条件値,値なしフラグ (必須項目なので値なしフラグは考慮しない)
			String[] params = modifyUserName.split(",", 2);
			criteria.addAttributeValueCriteria(attributeType, params[0]);
		}

		// 更新日
		if (modifyDate != null && modifyDate.length() > 0) {
			AttributeTypeDomain attributeType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_MODIFY_DATE"));
			attributeType.setValueType(ValueTypeEnum.DATE);
			// 最小値,最大値,値なしフラグ (必須項目なので値なしフラグは考慮しない)
			String[] params = modifyDate.split(",", 3);
			Date from = (params[0] != null && params[0].length() > 0) ? new Date(Long.valueOf(params[0])) : null;
			Date to = (params[1] != null && params[1].length() > 0) ? new Date(Long.valueOf(params[1])) : null;
			criteria.addAttributeRangeDateCriteria(attributeType, from, to);
		}

		// 作成者
		if (createUserName != null && createUserName.length() > 0) {
			AttributeTypeDomain attributeType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE"));
			attributeType.setValueType(ValueTypeEnum.STRING);
			// 条件値,値なしフラグ (必須項目なので値なしフラグは考慮しない)
			String[] params = createUserName.split(",", 2);
			criteria.addAttributeValueCriteria(attributeType, params[0]);
		}

		// 作成日
		if (createDate != null && createDate.length() > 0) {
			AttributeTypeDomain attributeType = new AttributeTypeDomain(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_CREATE_DATE"));
			attributeType.setValueType(ValueTypeEnum.DATE);
			// 最小値,最大値,値なしフラグ (必須項目なので値なしフラグは考慮しない)
			String[] params = createDate.split(",", 3);
			Date from = (params[0] != null && params[0].length() > 0) ? new Date(Long.valueOf(params[0])) : null;
			Date to = (params[1] != null && params[1].length() > 0) ? new Date(Long.valueOf(params[1])) : null;
			criteria.addAttributeRangeDateCriteria(attributeType, from, to);
		}

		// プロパティ
		if (property != null && property.length() > 0) {
			AttributeTypeDomain attributeType = new AttributeTypeDomain("プロパティ");
			attributeType.setValueType(ValueTypeEnum.STRING);
			// 条件値,値なしフラグ
			String[] params = property.split(",", 2);
			boolean isNullFlag = Boolean.valueOf(params[1]);

			if (isNullFlag) {
				// 値なし指定の場合
				criteria.addAttributeIsNullCriteria(attributeType);
			} else {
				// 値指定の場合
				criteria.addAttributeValueCriteria(attributeType, params[0]);
			}
		}

		// 有効期限
		if (effectiveTerm != null && effectiveTerm.length() > 0) {
			AttributeTypeDomain attributeType = new AttributeTypeDomain("有効期限");
			attributeType.setValueType(ValueTypeEnum.DATE);
			// 最小値,最大値,値なしフラグ
			String[] params = effectiveTerm.split(",", 3);
			boolean isNullFlag = Boolean.valueOf(params[2]);

			if (isNullFlag) {
				// 値なし指定の場合
				criteria.addAttributeIsNullCriteria(attributeType);
			} else {
				// 範囲指定の場合
				Date from = (params[0] != null && params[0].length() > 0) ? new Date(Long.valueOf(params[0])) : null;
				Date to = (params[1] != null && params[1].length() > 0) ? new Date(Long.valueOf(params[1])) : null;
				criteria.addAttributeRangeDateCriteria(attributeType, from, to);
			}
		}

		// 文書ID
		if (searchIndex != null && searchIndex.length() > 0) {
			AttributeTypeDomain attributeType = new AttributeTypeDomain("文書ID");
			attributeType.setValueType(ValueTypeEnum.STRING);
			// 条件値,値なしフラグ (必須項目なので値なしフラグは考慮しない)
			String[] params = searchIndex.split(",", 2);
			criteria.addAttributeValueCriteria(attributeType, params[0]);
		}

		// 番号(自動採番)
		if (autoNumber != null && autoNumber.length() > 0) {
			AttributeTypeDomain attributeType = new AttributeTypeDomain("番号");
			attributeType.setValueType(ValueTypeEnum.STRING);
			// 条件値,値なしフラグ (必須項目ではないが未対応のため値なしフラグは考慮しない)
			String[] params = autoNumber.split(",", 2);
			criteria.addAttributeValueCriteria(attributeType, params[0]);
		}

		// テーブルID
		if (tableId != null && !(tableId.equals("default"))) {
			criteria.setTableId(Long.valueOf(tableId));
		}

		// テーブル定義名
		criteria.setTableDefName(tableDefName);

		// 単一値拡張属性
		if (attType_Value != null && attType_Value.size() > 0) {
			for (String attTypeString : attType_Value) {
				// 属性タイプID,条件値,値なしフラグ
				String[] params = attTypeString.split(",", -1);
				Long attTypeId = Long.valueOf(params[0]);
				boolean isNullFlag = Boolean.valueOf(params[params.length - 1]);

				if (isNullFlag) {
					// 値なし指定の場合
					criteria.addAttributeIsNullCriteria(new AttributeTypeDomain(attTypeId));
				} else {
					// 単一値指定の場合
					String value = attTypeString.substring(attTypeString.indexOf(",") + 1, attTypeString.lastIndexOf(","));
					criteria.addAttributeValueCriteria(new AttributeTypeDomain(attTypeId), value);
				}
			}
		}

		// 数値型拡張属性
		if (attType_RangeNumeric != null && attType_RangeNumeric.size() > 0) {
			for (String attTypeString : attType_RangeNumeric) {
				// 属性タイプID,最小値,最大値,値なしフラグ
				String[] params = attTypeString.split(",", 4);
				Long attTypeId = Long.valueOf(params[0]);
				boolean isNullFlag = Boolean.valueOf(params[params.length - 1]);

				if (isNullFlag) {
					// 値なし指定の場合
					criteria.addAttributeIsNullCriteria(new AttributeTypeDomain(attTypeId));
				} else {
					// 範囲指定の場合
					criteria.addAttributeRangeNumericCriteria(new AttributeTypeDomain(attTypeId), toNumber(params[1]), toNumber(params[2]));
				}
			}
		}

		// 日時型拡張属性
		if (attType_RangeDate != null && attType_RangeDate.size() > 0) {
			for (String attTypeString : attType_RangeDate) {
				// 属性タイプID,最小値,最大値,値なしフラグ
				String[] params = attTypeString.split(",", 4);
				Long attTypeId = Long.valueOf(params[0]);
				boolean isNullFlag = Boolean.valueOf(params[params.length - 1]);

				if (isNullFlag) {
					// 値なし指定の場合
					criteria.addAttributeIsNullCriteria(new AttributeTypeDomain(attTypeId));
				} else {
					// 範囲指定の場合
					Date from = (params[1] != null && params[1].length() > 0) ? new Date(Long.valueOf(params[1])) : null;
					Date to = (params[2] != null && params[2].length() > 0) ? new Date(Long.valueOf(params[2])) : null;
					criteria.addAttributeRangeDateCriteria(new AttributeTypeDomain(attTypeId), from, to);
				}
			}
		}
		// TODO V6140ではドリルダウンには対応しないためファセットによる絞込み条件は一つとする
		// TODO V6140では空文字またはNULL値のファセットによる絞込みには対応しない

		// 単一値ファセット
		if (facet_Value != null && facet_Value.size() > 0) {
			for (String facetString : facet_Value) {
				// フィールド名,条件値
				String[] facetParams = facetString.split(",", 2);
				criteria.addFacetFieldValueCriteria(facetParams[0], facetParams[1]);
			}
		}

		// 数値(整数/実数)レンジファセット
		if (facet_RangeNumeric != null && facet_RangeNumeric.size() > 0) {
			// 数値範囲指定の場合は文字列として扱う
			for (String facetString : facet_RangeNumeric) {
				// フィールド名,最小値,最大値
				String[] facetParams = facetString.split(",", 3);
				String from = facetParams[1];
				String to = facetParams[2];
				criteria.addFacetFieldRangeNumericCriteria(facetParams[0], toNumber(from), toNumber(to));
			}
		}

		// 日時レンジファセット
		if (facet_RangeDate != null && facet_RangeDate.size() > 0) {
			for (String facetString : facet_RangeDate) {
				// フィールド名,最小値,最大値
				String[] facetParams = facetString.split(",", 3);
				Date from = (facetParams[1] != null && facetParams[1].length() > 0) ? new Date(Long.valueOf(facetParams[1])) : null;
				Date to = (facetParams[2] != null && facetParams[2].length() > 0) ? new Date(Long.valueOf(facetParams[2])) : null;
				criteria.addFacetFieldRangeDateCriteria(facetParams[0], from, to);
			}
		}

		// 表示タイプ (本文抜粋 or リスト or サムネイル)
		if (displayTypeId != null) {
			criteria.setDisplayType(DisplayTypeEnum.getById(Integer.valueOf(displayTypeId)));
		}

		return criteria;
	}

	/**
	 * 検索対象パスの「Garbage box」を「ごみ箱」に置換します。
	 * @param searchPath
	 * @return 置換後のsearchPath
	 * @throws Exception
	 */
	private String replaceSearchPathForEn(String searchPath) throws Exception {
		// スラッシュの数によりシステムごみ箱かWSごみ箱、どちらへの検索か特定する
		long slashCount = searchPath.chars().filter(c -> c == '/').count();
		int start = 0;

		if (slashCount == 2) {
			// システムごみ箱の検索のため第一階層が置換対象
			start = searchPath.indexOf("/") + 1;
		} else if (slashCount == 3) {
			// WSごみ箱の検索のため第二階層が置換対象
			start = searchPath.indexOf("/", searchPath.indexOf("/") + 1) + 1;
		}

		int end = searchPath.indexOf("/", start + 1);
		StringBuilder sb = new StringBuilder(searchPath);
		sb.replace(start, end, ConfigUtils.getByKey("OBJECT_TYPE_NAME_RECYCLE"));

		return sb.toString();
	}

	/**
	 * 文字列を数値型に変換して返却します。
	 * @param value
	 * @return
	 * @throws Exception
	 */
	private Number toNumber(String value) throws Exception {
		Number number = null;

		// パラメータが空の場合は変換しない
		if (value.equals("")){
			return null;
		}

		// 整数の正規表現
		String longRegex = "[+-]?\\d+";

		// 実数の正規表現
		String doubleRegex = "[+-]?(?:\\d+\\.?\\d*|\\.\\d+)";

		if (value.matches(longRegex)) {
			// 整数
			number = Long.valueOf(value);
		} else if (value.matches(doubleRegex)) {
			// 実数
			number = Double.valueOf(value);
		} else {
			throw new EIMApplicationException("EIM.ERROR.LOGIC.TO.NUMBER", new Object[]{value});
		}

		return number;
	}

	/**
	 * 検索結果を一覧画面表示用のDTOに変換します。
	 * @param searchResults
	 * @return
	 * @throws Exception
	 */
	private ContentSearchResultsDTO convertToDto(SearchResultsDomain searchResults) throws Exception {

		// ----------------------------------
		// 付加情報をDBから取得
		// ----------------------------------

		// 結果データのオブジェクトIDリストを取得
		List<Long> idList = new ArrayList<>();
		for (SearchRecordDomain record : searchResults.getRecordList()) {
			ValueFieldDomain<String> field = record.getValueField(ContentSearchFieldEnum.ID.toString());
			idList.add(Long.valueOf(field.getValue()));
		}

		// PDF変換オブジェクトを取得 (PDF変換中を示す情報は一時的な状態を表す付加情報なのでDBから取得する)
		Set<Long> pdfConversionObjectIdSet = publicDocumentService.getPDFConversionProcessingObjectIdSet(idList);

		// 常時読取り権限のあるオブジェクトを取得
		ObjectCriteria objectCriteria = new ObjectCriteria();
		MultipleCriteria<Long> ids = new MultipleCriteria<>(idList);
		objectCriteria.setIds(ids );
		objectCriteria.setAccessRoleType(new AccessRoleTypeDomain(AppConstant.ACCESS_ROLE_ALWAYS_READ));
		List<ObjectDomain> alwaysReadableList = objectDaoWithoutAttribute.getList(objectCriteria);
		Set<Long> alwaysReadableObjectIdSet = alwaysReadableList.stream().map(obj -> obj.getId()).collect(Collectors.toSet());

		// ----------------------------------
		// コンフィグレーションを取得
		// ----------------------------------

		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

		// 通知要否の設定を取得
		boolean isDisplayLatestLinkConfig = AppLogicUtil.isDisplayLatestLink(sess);

		// ----------------------------------
		// DTO生成
		// ----------------------------------

		ContentSearchResultsDTO resultDto = new ContentSearchResultsDTO();

		// ヒット件数設定
		resultDto.setNumFounds(searchResults.getNumFounds());

		// ----------------------------------
		// レコードDTO生成
		// ----------------------------------

		// 検索結果レコードをDTOに変換
		List<ContentSearchRecordDTO> recordDtoList = new ArrayList<>();
		resultDto.setContentList(recordDtoList);
		for (SearchRecordDomain record : searchResults.getRecordList()) {

			ContentSearchRecordDTO recordDto = new ContentSearchRecordDTO();
			recordDtoList.add(recordDto);

			// -----------------------------------------------
			// 検索結果フィールドデータを取得して返却値に設定
			// -----------------------------------------------

			Integer publicProcFailure = null;
			String statusTypeName = null;
			Date pdfConvExecDate = null;
			Date pdfPreRegistDate = null;
			String extension = null;
			boolean isFailedToPdfMerge = false;
			boolean isPublic = false;
			Integer revision = null;

			for (ValueFieldDomain<?> field : record.getValueFieldList()) {

				// フィールド名をキーにフィールド列挙値を取得 (多言語フィールドの場合セッション言語に対応するフィールドのみ対象となる)
				ContentSearchFieldEnum fieldEnum = ContentSearchFieldEnum.getByFieldName(field.getFieldName());

				// フィールド毎に値を取得
				if (fieldEnum != null) {
					// 既定フィールドの場合
					switch (fieldEnum) {
					case AUTO_NUMBER:
						recordDto.setNumber((String) field.getValue());
						break;
					case CREATE_DATE:
						recordDto.setCreateDateTime((Date) field.getValue());
						break;
					case CREATE_USER_LANG_ID_NAME:
						recordDto.setCreateUserName((String) field.getValue());
						break;
					case DATA_TYPE:
						recordDto.setObjTypeName(ContentTypeEnum.valueOf((String) field.getValue()).getValue());
						break;
					case EFFECTIVE_TERM:
						recordDto.setEffectiveTerm((Date) field.getValue());
						break;
					case EXTENSION:
						extension = (String) field.getValue();
						break;
					case FAILED_TO_PDF_MERGE:
						isFailedToPdfMerge = ((Integer) field.getValue()) == FlagValueEnum.ON.getValue() ? true : false;
						break;
					case FILE_SIZE:
						recordDto.setFileSize((Long) field.getValue());
						break;
					case ID:
						recordDto.setObjId(Long.valueOf((String) field.getValue()));
						break;
					case LATEST:
						recordDto.setIsLatest(((Integer) field.getValue()) == FlagValueEnum.ON.getValue() ? true : false);
						break;
					case LINK_PARENT_ID:
						// リンクの親フォルダID
						recordDto.setLinkParentObjId((Long) field.getValue());
						// ドキュメントリンクフラグ
						recordDto.setIsDocumentLink(true);
						break;
					case LINK_UPDATE_TIMING:
						// リンク更新タイミング (0:手動更新 1:公開時更新)
						recordDto.setDocumentLinkUpdateTiming((Integer) field.getValue());
						// ドキュメントリンクフラグ
						recordDto.setIsDocumentLink(true);
						break;
					case LOCK_DATE:
						recordDto.setLockDate((Date) field.getValue());
						break;
					case LOCK_USER_LANG_ID_NAME:
						recordDto.setLockUserName((String) field.getValue());
						break;
					case MODIFY_DATE:
						recordDto.setModifyDateTime((Date) field.getValue());
						break;
					case MODIFY_USER_LANG_ID_NAME:
						recordDto.setModifyUserName((String) field.getValue());
						break;
					case OCR_PROC_STATUS:
						recordDto.setOcrProcessStatus(String.valueOf((Integer) field.getValue()));
						break;
					case OCR_RESULT_STATUS:
						recordDto.setOcrResultStatus(String.valueOf((Integer) field.getValue()));
						break;
					case OBJECT_NAME:
						recordDto.setObjName((String) field.getValue());
						break;
					case PATH:
						recordDto.setPath((String) field.getValue());
						break;
					case PDF_CONV_EXEC_DATE:
						pdfConvExecDate = (Date) field.getValue();
						break;
					case PDF_PRE_REGIST_DATE:
						pdfPreRegistDate = (Date) field.getValue();
						break;
					case PROPERTY:
						recordDto.setProperty((String) field.getValue());
						break;
					case PUBLIC:
						// 公開済み (有効な公開ファイルを保持している場合にtrueとする)
						isPublic = (Integer) field.getValue() == FlagValueEnum.ON.getValue() ? true : false;
						break;
					case PUBLIC_PROC_FAILURE:
						publicProcFailure = (Integer) field.getValue();
						break;
					case REVISION:
						revision = (Integer) field.getValue();
						break;
					case SIGNENCR:
						recordDto.setSignencr((Integer) field.getValue());
						break;
					case STATUSTYPE_KIND:
						recordDto.setStatusTypeKind((Integer) field.getValue());
						break;
					case STATUSTYPE_LANG_ID_NAME:
						statusTypeName = (String) field.getValue();
						break;
					case WF_ATTACHED_FOLDER_ID:
						recordDto.setHigherWFFolder((Long) field.getValue());
						break;
					case MATCH_PAGES:
						// ページアイコン表示フラグ
						recordDto.setIsDspPageIcon(((Integer) field.getValue()) == FlagValueEnum.ON.getValue() ? true : false);
						break;
					default:
						break;
					}
				} else {
					// カスタムフィールドの場合
					String responseParamName = convertToParamName(field);
					recordDto.addAttr(responseParamName, field.getValue());
				}
			}

			// --------------------------------------
			// 加工が必要な返却値設定
			// --------------------------------------

			// ドキュメントフラグ
			boolean isDocument = recordDto.getObjTypeName().equals(ContentTypeEnum.DOCUMENT.getValue());
			if (isDocument) {
				recordDto.setIsDocument(true);
			}

			// レビジョン
			if (isDocument) {
				recordDto.setRev(revision);
			}

			// 公開のみ参照可
			if (!alwaysReadableObjectIdSet.contains(recordDto.getObjId())) {
				// 常時読取り権限が無い場合
				recordDto.setReadOnly(true);
			}

			// 公開済みフラグ
			if (recordDto.getStatusTypeKind() != null && recordDto.getStatusTypeKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC) {
				// ステータス種別が「公開済み」の場合
				recordDto.setIsPublished(true);
			}

			// PDF変換ステータス
			int pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_NONE;
			if (isDocument) {
				// ドキュメントの場合
				if (recordDto.getStatusTypeKind() != null) {
					// ステータスがある場合
					if (isPublic
							&& (publicProcFailure == null || publicProcFailure == FlagValueEnum.OFF.getValue())
							&& (pdfConvExecDate != null && pdfConvExecDate.getTime() > recordDto.getModifyDateTime().getTime())) {
						// 「変換完了（原本ファイルと乖離なし）」
						//		公開ファイル有
						//		PDF変換失敗フラグ無かOFF
						//		PDF変換日時が更新日時より後
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL;
					} else if (isPublic
							&& (publicProcFailure == null || publicProcFailure == FlagValueEnum.OFF.getValue())
							&& (pdfConvExecDate != null && pdfConvExecDate.getTime() <= recordDto.getModifyDateTime().getTime())) {
						// 「変換完了（原本ファイルと乖離あり）」
						//		公開ファイル有
						//		PDF変換失敗フラグ無かOFF
						//		PDF変換日時が更新日時以前
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL;
					} else if (pdfConversionObjectIdSet.contains(recordDto.getObjId())) {
						// 「変換中」
						//		PDF変換オブジェクト有
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESSING;
					} else if ((publicProcFailure != null && publicProcFailure == FlagValueEnum.ON.getValue())
							&& (pdfConvExecDate == null || pdfConvExecDate.getTime() > recordDto.getModifyDateTime().getTime())) {
						// 「変換失敗」
						//		PDF変換失敗フラグON
						//		PDF変換日時無かPDF変換日時が更新日時より後
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_FAILURE;
					}
					// 公開PDF事前登録日時が存在する場合はその判定を優先する
					if (pdfPreRegistDate != null && pdfPreRegistDate.getTime() <= recordDto.getModifyDateTime().getTime()) {
						// 公開PDF事前登録日時がオブジェクトのMDATE（原本ファイルの更新日時）以前
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL;
					} else if (pdfPreRegistDate != null && pdfPreRegistDate.getTime() > recordDto.getModifyDateTime().getTime()) {
						pdfConversionStatus = AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL;
					}
				}
				recordDto.setIsPdfPreRegistered(pdfPreRegistDate != null ? true : false);
				recordDto.setPdfConversionStatus(pdfConversionStatus);
			}

			// WF付きフォルダフラグ
			if (recordDto.getObjTypeName().equals(ContentTypeEnum.FOLDER.getValue())
					&& recordDto.getStatusTypeKind() != null
					&& recordDto.getHigherWFFolder() == null) {
				recordDto.setIsWorkflowFolder(true);
			}

			// ステータス表記
			String statusTypeDisplayName = null;
			if (isDocument) {
				// ドキュメントの場合
				if (isFailedToPdfMerge) {
					// 結合処理失敗状態の場合「結合処理失敗」
					statusTypeDisplayName = ResourceUtils.getByKey("EIM.LABEL.PDF.JOIN.FAILED");
				} else if (pdfConversionStatus == AppConstant.PDF_CONVERSION_STATUS_PROCESSING
						&& recordDto.getStatusTypeKind() != AppConstant.STATUS_TYPE_KIND_ID_PROCESSING_PUBLIC) {
					// 公開処理中以外の承認ステータスでPDF変換中の場合「(PDF変換中)」を付加
					statusTypeDisplayName = statusTypeName + ResourceUtils.getByKey("EIM.LABEL.PDF.CONVERT.PROCESSING");
				} else if (pdfConversionStatus == AppConstant.PDF_CONVERSION_STATUS_PROCESS_FAILURE) {
					// PDF変換失敗状態の場合「(PDF変換失敗)」を付加
					statusTypeDisplayName = statusTypeName + ResourceUtils.getByKey("EIM.LABEL.PDF.CONVERT.FAILED");
				} else if (recordDto.getStatusTypeKind() == null) {
					// ステータスなし文書の場合「- (ステータスなし)」
					statusTypeDisplayName = "-";
				} else {
					// 上記以外は承認ステータスをそのまま表記
					statusTypeDisplayName = statusTypeName;
				}
			} else if (recordDto.getIsWorkflowFolder()) {
				// WF付きフォルダはそのまま表記
				statusTypeDisplayName = statusTypeName;
			} else {
				// - (ステータスなし)
				statusTypeDisplayName = "-";
			}
			recordDto.setStatusTypeName(statusTypeDisplayName);

			// PDFアイコン表示フラグ
			if (pdfConversionStatus == AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_NOT_SAME_ORIGINAL
					|| pdfConversionStatus == AppConstant.PDF_CONVERSION_STATUS_PROCESS_COMPLETE_SAME_ORIGINAL
					|| (extension != null && extension.equalsIgnoreCase(FileTypeEnum.PDF.getValue()) && isPublic)) {
				// PDF変換完了、または原本がPDF且つ公開済み(公開ファイルが存在)の場合
				recordDto.setIsDspPdfIcon(true);
			}
			// TODO 改訂直後のWFなしPDFの公開アイコンが表示されてしまう(既存バグ？)

			// WFなしドキュメントの公開アイコン表示フラグ
			if (isDocument && recordDto.getStatusTypeKind() == null && isPublic) {
				// WFなしドキュメントで有効な公開ファイルを保持している場合
				recordDto.setIsDspPubIconForNoWF(true);
			}

			// TODO actSearch.jspではOCR処理ステータス表示フラグ。dspChildObject.jspと用途が異なる？不要と思われるので削除した方がいい。
//			// WFなしドキュメントの公開フラグ
//			if (isDocument && !isFailedToPdfMerge) {
//				// ドキュメント且つ結合失敗でない
//				recordDto.setIsNoWFPublic(true);
//			}

			// 上位階層ワークフロー付きフォルダステータス種別 (上位階層ワークフロー付きフォルダステータス種別は子と同じになる)
			if (recordDto.getHigherWFFolder() != null && recordDto.getHigherWFFolder() > 0) {
				recordDto.setHigherWFFolderStatusTypeKind(recordDto.getStatusTypeKind());
			}

			// 有効期限切れ
			Date effectiveTerm = recordDto.getEffectiveTerm();
			if(effectiveTerm != null)
			{
				recordDto.setExpiration(DateUtils.judgeExpirationDate(sess, recordDto.getEffectiveTerm()));
			}

			// リンクドキュメント過去版警告通知 (リンクしているドキュメントが過去版であることを通知するフラグ)
			if (recordDto.getIsDocumentLink()) {
				if (isDisplayLatestLinkConfig && !recordDto.getIsLatest()) {
					// 通知設定がtrueでリンクしているドキュメントが過去版の場合
					recordDto.setIsDspLatestLink(true);
				}
			}

			// --------------------------------------
			// スニペット設定
			// --------------------------------------

			if (record.getSnippetFieldList() != null) {
				for (SnippetFieldDomain snippetField : record.getSnippetFieldList()) {
					switch (snippetField.getValueType()) {
					case ATTRIBUTE:
						// 属性値スニペット
						for (String snippet : snippetField.getSnippetList()) {
							recordDto.addSnippetAttribute(snippet);
						}
						break;
					case FULL_TEXT:
						// ファイル全文スニペット
						for (String snippet : snippetField.getSnippetList()) {
							recordDto.addSnippetFullText(snippet);
						}
						break;
					}
				}
			}
		}

		// --------------------------------------
		// ファセット設定
		// --------------------------------------

		resultDto.setFacetFieldList(searchResults.getFacetFieldList());

		return resultDto;
	}

	/**
	 * 検索エンジンのフィールド名をレスポンスデータのパラメータ名に変換する
	 * @param fieldName フィールド名
	 * @return レスポンスデータパラメータ名(フィールド名プレフィックス_属性タイプID_データ型サフィックス)
	 */
	private String convertToParamName(ValueFieldDomain<?> field) {

		// フィールド名プレフィックス_属性タイプID_データ型サフィックス
		String[] separatedFieldName = field.getFieldName().split("_");

		// リスト値の場合はパラメータ名に複数値サフィックスを付与
		String suffix = (field.getValue() instanceof List) ? PARAM_NAME_SUFFIX_MULTI_VALUE : "";

		// パラメータ名プレフィックス_属性タイプID_複数値サフィックス
		String paramName = PARAM_NAME_PREFIX_CUSTOM_ATTRIBUTE + separatedFieldName[1] + suffix;

		return paramName;

	}

	// ------------------------
	// getter/setter
	// ------------------------

	/**
	 * 公開ドキュメントサービスを取得します。
	 * @return 公開ドキュメントサービス
	 */
	public PublicDocumentService getPublicDocumentService() {
		return publicDocumentService;
	}

	/**
	 * 公開ドキュメントサービスを設定します。
	 * @param publicDocumentService 公開ドキュメントサービス
	 */
	public void setPublicDocumentService(PublicDocumentService publicDocumentService) {
		this.publicDocumentService = publicDocumentService;
	}

	/**
	 * オブジェクトサービスを取得します。
	 * @return オブジェクトサービス
	 */
	public ObjectService getObjectService() {
		return objectDaoWithoutAttribute;
	}

	/**
	 * オブジェクトサービスを設定します。
	 * @param objectService オブジェクトサービス
	 */
	public void setObjectService(ObjectService objectService) {
		this.objectDaoWithoutAttribute = objectService;
	}

}