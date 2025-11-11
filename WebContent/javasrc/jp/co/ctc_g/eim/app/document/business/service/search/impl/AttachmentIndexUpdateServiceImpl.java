package jp.co.ctc_g.eim.app.document.business.service.search.impl;

import java.sql.Array;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.solr.client.solrj.SolrServerException;

import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.bo.EIMSearchLimitCountCondition;
import eim.bo.EIMSearchResultList;
import eim.bo.EIMSearchSelectEIMObject;
import eim.bo.EIMSearchSelectEIMObject.PsedoAttributeTypeEnum;
import eim.db.DBUtils;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.SearchUtils;
import eim.util.TypeConvertUtils;
import eim.util.internal.search.ValueTypeEnum;
import jp.co.ctc_g.eim.app.document.common.enumeration.search.ContentSearchFieldEnum;
import jp.co.ctc_g.eim.app.document.common.enumeration.search.ContentTypeEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.common.context.EIMThreadContext;
import jp.co.ctc_g.eim.framework2.common.context.TransactionContext;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.framework2.integration.dao.plugin.DatabasePlugInLoader;
import jp.co.ctc_g.eim.search.core.common.util.SearchConstant;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.IndexDataDomain;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.UpdateResultDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchRecordDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchResultsDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.ValueFieldDomain;
import jp.co.ctc_g.eim.search.solr.common.enumeration.ObjectSchemaFieldEnum;
import jp.co.ctc_g.eim.search.solr.indexBase.business.domain.FullTextDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.domain.SolrFieldDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.domain.SolrIndexDataDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.service.impl.SolrBaseIndexUpdateServiceImpl;
import jp.co.ctc_g.eim.search.solr.indexBase.business.service.impl.SolrDataOutputServiceImpl;
import jp.co.ctc_g.eim.search.solr.searchApi.business.SolrSearchDao;
import jp.co.ctc_g.eim.search.solr.searchApi.business.domain.criteria.SolrSearchCriteria;
import jp.co.ctc_g.eim.search.solr.searchApi.business.domain.criteria.SolrSearchValueListCriteria;

/**
 * 検索エンジンの添付ファイル全文テキストフィールドの更新処理を行うサービス実装クラスです。
 */
public class AttachmentIndexUpdateServiceImpl extends SolrBaseIndexUpdateServiceImpl {

	/** Logger */
	private static final Log log = LogFactory.getLog(SolrDataOutputServiceImpl.class);

	/** アプリケーション種別（ドキュメント管理） */
	protected final String APPLICATION_KIND = EIMConfig.get("APPLICATION_KIND");

	/** 帳票添付ファイルのオブジェクトタイプ名 */
	private final String OBJECT_TYPE_NAME_FORM_ATTACH_FILE = EIMConfig.get("OBJECT_TYPE_NAME_FORM_ATTACH_FILE");

	/** パス属性の属性名 */
	protected final String PATH = EIMConfig.get("ATTR_NAME_DOCUMENT_PASS");

	/** 検索Dao */
	private SolrSearchDao searchDao = null;

	// ------------------
	// Publicメソッド
	// ------------------

	/**
	 * 添付ファイルが新規登録または削除された時、それを参照する親オブジェクトの添付ファイル全文テキストフィールドを更新します。
	 * <p>
	 * 更新対象フィールド：
	 * <ul>
	 * <li>ATTACHMENT_IDS</li>
	 * <li>ATTACHMENT_FULL_TEXTS</li>
	 * </ul>
	 * 他のコンテンツデータはデータ収集プラグインでインデックスデータを生成しているが、添付ファイル更新時はここで生成します。<br>
	 * それは添付ファイル更新通知は添付ファイルオブジェクトですが、実際に更新するのはドキュメントのインデックスレコードです。<br>
	 * そのため、ここでドキュメントのインデックス更新データを生成しています。
	 * <p>
	 * @throws Exception
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.impl.SolrAttributesIndexUpdateServiceImpl#update(java.util.List)
	 */
	@Override
	public void update(List<IndexDataDomain> indexDataList) throws Exception {

		// -----------------------------------------------
		// 更新対象インデックスデータを更新種別毎に分類
		// -----------------------------------------------

		// 更新種別が「インデックス新規作成・更新」
		List<IndexDataDomain> updateIndexDataList = indexDataList.stream()
				.filter(data -> data.getUpdateKind().equals(SearchConstant.UPDATECODE_INDEX_CRE_UPD)).collect(Collectors.toList());

		// 添付ファイルIDとインデックス情報ドメインのMapを保持
		Map<Long, IndexDataDomain> updateIdToIndexDataMap = updateIndexDataList.stream()
				.collect(Collectors.toMap(data -> Long.valueOf(data.getId()), data -> data));

		// 更新種別が「インデックス削除」
		List<IndexDataDomain> deleteIndexDataList = indexDataList.stream()
				.filter(data -> data.getUpdateKind().equals(SearchConstant.UPDATECODE_INDEX_DEL)).collect(Collectors.toList());

		// ---------------------------------------------------------------------------------
		// 更新対象インデックスデータをキーに登録済の親ドキュメントをSolrから取得 (削除時)
		// ---------------------------------------------------------------------------------

		// 検索実行
		List<SearchRecordDomain> existingParentRecordList = getExistingParentRecordListByIndexList(deleteIndexDataList);

		// 親ドキュメントのIDのSet
		Set<Long> existingParentIdSet = new HashSet<>();
		for (SearchRecordDomain record : existingParentRecordList) {
			ValueFieldDomain<String> field = record.getValueField(ContentSearchFieldEnum.ID.toString());
			existingParentIdSet.add(Long.valueOf(field.getValue()));
		}

		// --------------------------------------------------------------------------
		// 更新対象インデックスデータをキーに親ドキュメントをDBから取得 (更新時)
		// --------------------------------------------------------------------------

		// 親ドキュメントID一覧を取得
		List<Long> parentIdList = getParentIdList(updateIndexDataList);

		// 存在チェックのためにDBから取得した親ドキュメントのIDをキーに登録済の親ドキュメント一覧をSolrから取得
		existingParentRecordList.addAll(getExistingParentRecordListByIdList(parentIdList));

		// 存在チェックのために登録済みオブジェクトインデックスのキー(IDとSeqリストの組み合わせ)を保持
		Map<String, List<Integer>> idToSeqListMap = new HashMap<>();
		for (SearchRecordDomain existingParentRecord : existingParentRecordList) {
			String id = (String) existingParentRecord.getValueField(ObjectSchemaFieldEnum.ID.toString()).getValue();
			Integer seq = existingParentRecord.getValueField(ObjectSchemaFieldEnum.SEQ.toString()) != null
					? (Integer) existingParentRecord.getValueField(ObjectSchemaFieldEnum.SEQ.toString()).getValue() : 0;

			if (!idToSeqListMap.containsKey(id)) {
				idToSeqListMap.put(id, new ArrayList<>());
			}
			idToSeqListMap.get(id).add(seq);
		}

		// -----------------------------------------------------------------------------
		// 削除対象と更新対象をマージして更新対象の親ドキュメント一覧をDBから再取得
		// -----------------------------------------------------------------------------

		// 登録済の親ドキュメントが存在すればそれを追加
		for (Long id : existingParentIdSet) {
			if (!parentIdList.contains(id)) {
				parentIdList.add(id);
			}
		}

		// 属性を含めて添付ファイルを保持する親ドキュメント一覧をDBから再取得
		List<EIMObject> parentObjectList = getParentObjectList(parentIdList);

		// ----------------------------------------
		// 親ドキュメントのインデックスデータに変換
		// ----------------------------------------

		// 親ドキュメントのインデックスデータを生成
		for (EIMObject parent : parentObjectList) {

			// パス属性の数分ループしてドキュメントリンクに対してもインデックスデータを生成
			EIMAttribute pathAttr = parent.getAttribute(PATH);
			if (pathAttr == null) {
				// オブジェクトのパス属性が取得できません。
				log.warn(ResourceUtils.getByKey("EIM.ERROR.LOGIC.NO.PATH.ATTRIBUTE", parent.getId()));
				continue;
			}

			// 同一ドキュメント(リンク)内で全文テキストを共有するための一時保管用Map
			Map <Long, String> attachmentIdToFullTextMap = new HashMap<>();

			for (int seq = 0; seq < pathAttr.getStrings().length; seq ++) {

				// オブジェクトインデックスが未登録の場合はスキップ
				if (!idToSeqListMap.containsKey(String.valueOf(parent.getId()))
						|| !idToSeqListMap.get(String.valueOf(parent.getId())).contains(seq)) {
					log.warn(String.format("添付ファイル全文インデックス更新対象のオブジェクトインデックスが存在しないため更新できません。[id:%d][seq:%d]"
							, parent.getId(), seq));
					continue;
				}

				// Solr用インデックスデータを生成して返却リストに追加
				SolrIndexDataDomain newIndexData = new SolrIndexDataDomain(String.valueOf(parent.getId()),
						SearchConstant.UPDATECODE_INDEX_CRE_UPD, ContentTypeEnum.DOCUMENT.toString(),
						SearchConstant.DATAKIND_TX_OBJECT, ContentTypeEnum.DOCUMENT.toString());
				newIndexData.setSeq(seq);
				newIndexData.setFieldList(new ArrayList<>());

				// アプリケーション種別に「DOCUMENT_MANAGEMENT」を設定
				newIndexData.setApplicationKind(new String[]{APPLICATION_KIND});

				// 添付ファイル
				@SuppressWarnings("unchecked")
				List<EIMAttribute> attrList = parent.getAttributeList();

				// 添付ファイル属性の抽出
				List<EIMAttribute> attachmentAttrList = attrList.stream()
						// オブジェクト型
						.filter(attr -> attr.getType().getValueType().getId() == ValueTypeEnum.OBJECT.getId())
						// 複数値
						.filter(attr -> attr.getType().isMultiple() == true)
						// オブジェクトのクラスが「app.form.dev:帳票添付ファイル」
						.filter(attr -> attr.getObjects()[0].getType().getDefName().equals(OBJECT_TYPE_NAME_FORM_ATTACH_FILE))
						.collect(Collectors.toList());

				// 添付ファイルIDを取得
				List<Long> attachmentIdList = new ArrayList<>();
				for (EIMAttribute attachmentAttr : attachmentAttrList) {
					for (EIMObject attachment : attachmentAttr.getObjects()) {
						attachmentIdList.add((long)attachment.getId());
					}
				}

				// 添付ファイルID
				Long[] attachmentIds = attachmentIdList.toArray(new Long[attachmentIdList.size()]);
				newIndexData.getFieldList().add(new SolrFieldDomain(ContentSearchFieldEnum.ATTACHMENT_IDS.toString(), attachmentIds));

				// 添付ファイル全文
				List<String> fullTextList = new ArrayList<>();
				boolean existingParentRetrieved = false;
				for (Long attachmentId : attachmentIdList) {

					// ドキュメント本体に対する添付ファイル全文テキストを取得 (ドキュメントリンクの場合は共用する)
					if (seq == 0) {
						if (updateIdToIndexDataMap.containsKey(attachmentId)) {
							// 更新されたデータの場合はテキスト抽出結果を取得
							String fullText = extractFullText((SolrIndexDataDomain) updateIdToIndexDataMap.get(attachmentId));
							attachmentIdToFullTextMap.put(attachmentId, fullText);
						} else if (!existingParentRetrieved) {
							// 更新されたデータでない場合でSolrレコードが未取得の場合は登録済みのテキストをSolrから取得
							existingParentRetrieved = true;

							// Solrから登録済みの親ドキュメントを取得
							SearchRecordDomain record = getExistingParentById(String.valueOf(parent.getId()));

							if (record != null) {
								// 添付ファイルID
								ValueFieldDomain<List<Long>> attachmentIdsField =
										record.getValueField(ContentSearchFieldEnum.ATTACHMENT_IDS.toString());
								List<Long> existingAttachmentIdList = attachmentIdsField != null ? attachmentIdsField.getValue() : new ArrayList<>();

								// 添付ファイル全文テキスト
								ValueFieldDomain<List<String>> attachmentFullTextsField =
										record.getValueField(ContentSearchFieldEnum.ATTACHMENT_FULL_TEXTS.toString());
								List<String> existingAttachmentFullTextList = attachmentFullTextsField != null ? attachmentFullTextsField.getValue() : new ArrayList<>();

								// 添付ファイルID:全文テキストをMapに保管
								for (int i = 0; i < existingAttachmentIdList.size(); i ++) {
									if (existingAttachmentFullTextList.size() > i) {
										// テキストが未取得の場合にMapに追加
										attachmentIdToFullTextMap.putIfAbsent(existingAttachmentIdList.get(i), existingAttachmentFullTextList.get(i));
									}
								}
							}
						}
					}

					// 添付ファイル全文テキストを複数値リストに追加
					String fullText = attachmentIdToFullTextMap.containsKey(attachmentId) ? attachmentIdToFullTextMap.get(attachmentId) : "";
					fullTextList.add(fullText);
				}
				String[] fullTexts = fullTextList.toArray(new String[fullTextList.size()]);
				newIndexData.getFieldList().add(new SolrFieldDomain(ContentSearchFieldEnum.ATTACHMENT_FULL_TEXTS.toString(), fullTexts));

				// ----------------------------------------
				// 検索インデックス更新
				// ----------------------------------------

				// 添付ファイル内容全文のインデックス登録
				UpdateResultDomain result = objectIndexUpdateDao.updateAtomic(newIndexData);
				log.info(EIMResource.getMessage("EIM.INFO.LOGIC.EXECUTED.ATTACHMENT.FULLTEXT.FIELD.UPDATE", new Object[]{newIndexData.getId(), result.getStatus()}));
			}
		}
	}

	/**
	 * 添付ファイルが削除された時、それを参照する親オブジェクトの添付ファイル全文テキストフィールドを更新する。
	 * <p>
	 * 更新対象フィールド：
	 * <ul>
	 * <li>ATTACHMENT_IDS</li>
	 * <li>ATTACHMENT_FULL_TEXTS</li>
	 * </ul>
	 * update()に処理移譲している。<p>
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.impl.SolrAttributesIndexUpdateServiceImpl#delete(java.util.List)
	 */
	@Override
	public void delete(List<IndexDataDomain> indexDataList) throws Exception {

		// Solr用インデックスデータをコピー生成
		List<IndexDataDomain> solrIndexDataList = new ArrayList<>();
		for (IndexDataDomain indexData : indexDataList) {
			SolrIndexDataDomain solrIndexData = new SolrIndexDataDomain(indexData.getId(), indexData.getUpdateKind(),
					indexData.getSystemKind(), indexData.getDataKind(), indexData.getDataType());
			solrIndexDataList.add(solrIndexData);
		}

		// 更新処理の呼び出し
		update(solrIndexDataList);
	}

	// ------------------
	// Privateメソッド
	// ------------------

	/**
	 * 更新対象を参照している親オブジェクトID一覧をDBから取得
	 * @param indexDataList インデックス情報のリスト
	 * @return 親オブジェクトIDのリスト
	 * @throws Exception
	 */
	private List< Long> getParentIdList(List<IndexDataDomain> indexDataList) throws Exception {

		if (indexDataList == null || indexDataList.size() == 0) {
			return new ArrayList<>();
		}

		TransactionContext tx = EIMThreadContext.getTransactionContext();

		// Connection
		Connection conn = tx.getDBConnection();

		// Statement
		PreparedStatement pstmt = null;

		// ResultSet
		ResultSet rs = null;

		// SQL
		String sql =
				"select " +
					"EIMOBJREF.value, EIMOBJ.id " +
				"from " +
					"EIMOBJ inner join EIMOBJREF on eimobj.id = EIMOBJREF.id " +
				"where " +
					"EIMOBJREF.value in (" + 
					DatabasePlugInLoader.getPlugIn().getQueryStringSelectArray(Types.BIGINT) +
					")";

		// オブジェクトIDリスト
		List<Long> idList = indexDataList.stream().map(obj -> Long.valueOf(obj.getId())).collect(Collectors.toList());

		try {
			pstmt = conn.prepareStatement(sql);
			Connection oc = DBUtils.getNativeConnection(conn);
			Array array = DatabasePlugInLoader.getPlugIn().createArray(oc, idList.toArray(new Long[0]));
			pstmt.setArray(1, array);

			// Execute
			rs = pstmt.executeQuery();

			// Result
			Set<Long> objectIdSet = new HashSet<>();
			while (rs.next()) {
				objectIdSet.add(rs.getLong("id"));
			}

			return new ArrayList<>(objectIdSet);

		} finally {
			if (rs != null) {
				rs.close();
			}
			if (pstmt != null) {
				// Close Statement
				pstmt.close();
			}
		}
	}

	/**
	 * 属性を含めて親オブジェクト一覧をDBから取得
	 * @param idList 親オブジェクトIDのリスト
	 * @return 親オブジェクトのリスト
	 * @throws Exception
	 */
	private List<EIMObject> getParentObjectList(List<Long> idList) throws Exception {

		if (idList == null || idList.size() == 0) {
			return new ArrayList<>();
		}

		// Session
		EIMSession sess = jp.co.ctc_g.eim.framework.common.util.EIMThreadContext.getEIMSession();

		EIMSearchSelectEIMObject selectTarget = new EIMSearchSelectEIMObject();

		// 取得対象オブジェクトID配列
		long[] targetIds = idList.stream().mapToLong(id -> id).toArray();

		// 検索条件の設定
		EIMSearchSelectEIMObject.SearchConditionBuildHelper h = new EIMSearchSelectEIMObject.SearchConditionBuildHelper();
		selectTarget.setCondition(h.group(h.opAnd())
				.addCondition(
						h.in(h.opAnd(),
								PsedoAttributeTypeEnum.ID,
								h.opIn(),
								TypeConvertUtils.convertToBuildTypeArray(targetIds))	// ID：検索対象のID(配列)
				)
		);

		// 検索の実行・取得件数は無制限
		EIMSearchResultList resultList = SearchUtils.searchObjects(sess, selectTarget,
				new EIMSearchLimitCountCondition(SearchLimitCountCondition.UNLIMITED, false));

		@SuppressWarnings("unchecked")
		List<EIMObject> objectList = resultList;

		return objectList;
	}

	/**
	 * 検索インデックス登録済の親ドキュメント一覧を取得します。
	 * @param indexDatalist 検索インデックス登録対象の添付ファイルインデックスデータ
	 * @return 検索インデックス登録済の親ドキュメント一覧
	 * @throws Exception
	 */
	private List<SearchRecordDomain> getExistingParentRecordListByIndexList(List<IndexDataDomain> indexDatalist) throws Exception {

		if (indexDatalist == null || indexDatalist.size() == 0) {
			return new ArrayList<>();
		}

		SolrSearchValueListCriteria<Long> criteria = new SolrSearchValueListCriteria<Long>();

		// 条件フィールド
		criteria.setFieldName(ContentSearchFieldEnum.ATTACHMENT_IDS.toString());

		// 条件値リスト
		List<Long> idList = indexDatalist.stream().map(index -> Long.valueOf(index.getId())).collect(Collectors.toList());
		criteria.setValueList(idList);

		// 取得フィールド
		criteria.setResultFieldNameList(Arrays.asList(
				ContentSearchFieldEnum.ID.toString(),
				ContentSearchFieldEnum.SEQ.toString()));

		// 検索実行
		List<SearchRecordDomain> resultRecordList = searchDao.searchByValueList(criteria);

		return resultRecordList;

	}

	/**
	 * 検索インデックス登録済の親ドキュメントリストを取得します。
	 * @param idList 親ドキュメントのオブジェクトIDリスト
	 * @return 検索インデックス登録済の親ドキュメント一覧
	 * @throws Exception
	 */
	private List<SearchRecordDomain> getExistingParentRecordListByIdList(List<Long> idList) throws Exception {

		if (idList == null || idList.size() == 0) {
			return new ArrayList<>();
		}

		SolrSearchValueListCriteria<String> criteria = new SolrSearchValueListCriteria<String>();

		// 条件フィールド
		criteria.setFieldName(ContentSearchFieldEnum.ID.toString());

		// 条件値リスト
		List<String> strIdList = idList.stream().map(id -> String.valueOf(id)).collect(Collectors.toList());
		criteria.setValueList(strIdList);

		// 取得フィールド
		criteria.setResultFieldNameList(Arrays.asList(
				ContentSearchFieldEnum.ID.toString(),
				ContentSearchFieldEnum.SEQ.toString()));

		// 検索実行
		List<SearchRecordDomain> resultRecordList = searchDao.searchByValueList(criteria);

		return resultRecordList;

	}

	/**
	 * 添付ファイルの全文テキストを抽出して返却します。
	 * @param solrIndexData インデックス情報ドメイン
	 * @return 添付ファイルの全文テキスト
	 * @throws Exception
	 */
	private String extractFullText(SolrIndexDataDomain solrIndexData) throws Exception {

		if (solrIndexData == null) {
			return "";
		}

		// 実ファイルパス
		String filePath = solrIndexData.getRealFilePath();

		if (filePath == null || filePath.length() == 0) {
			// 実ファイルパスの指定がない場合(全文インデックス生成対象外)は空で更新する
			log.info(EIMResource.getMessage("EIM.ERROR.INPUT.NO.REAL.FILE.PATH.SPECIFIED", new Object[]{solrIndexData.getId()}));
			return "";
		}

		try {
			// テキスト抽出処理
			FullTextDomain fullText = objectIndexUpdateDao.extractFullText(filePath);

			return fullText.getText();
		} catch (Exception e) {
			// Solrとの通信エラーの場合は例外をthrow (更新通知が残るので再処理対象になる)
			if (e.getCause() instanceof SolrServerException) {
				throw e;
			}

			// その他の例外はログを出力して処理継続 (更新通知は削除される)
			log.warn(e.getMessage(), e);

			return "";
		}
	}

	/**
	 * 親ドキュメントIDをキーに登録済みの親ドキュメントをSolrから取得します。
	 * @param parentId 親ドキュメントID
	 * @return 親ドキュメントレコード
	 * @throws Exception
	 */
	private SearchRecordDomain getExistingParentById(String parentId) throws Exception {

		SolrSearchCriteria criteria = new SolrSearchCriteria();

		// 検索条件
		criteria
			.addConditionString(ContentSearchFieldEnum.ID.toString(), parentId)
			.addConditionNumeric(ContentSearchFieldEnum.SEQ.toString(), 0);

		// 取得フィールド
		criteria.setResultFieldNameList(Arrays.asList(
				ContentSearchFieldEnum.ID.toString(),
				ContentSearchFieldEnum.SEQ.toString(),
				ContentSearchFieldEnum.ATTACHMENT_IDS.toString(),
				ContentSearchFieldEnum.ATTACHMENT_FULL_TEXTS.toString()));

		// 取得件数
		criteria.setResultRows(1);

		// アクセス権限を無視
		criteria.setIgnoreAuthorization(true);

		// 検索実行
		SearchResultsDomain searchResults = searchDao.search(criteria);

		if (searchResults.getRecordList().size() > 0) {
			// ヒットした1件目のレコードを返却
			return searchResults.getRecordList().get(0);
		} else {
			return null;
		}

	}

	// ------------------
	// getter/setter
	// ------------------

	/**
	 * searchDaoを取得します。
	 * @return searchDao
	 */
	public SolrSearchDao getSearchDao() {
		return searchDao;
	}

	/**
	 * searchDaoを設定します。
	 * @param searchDao searchDao
	 */
	public void setSearchDao(SolrSearchDao searchDao) {
		this.searchDao = searchDao;
	}

}
