package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.ObjectUtils;

import common.util.AppConstant;
import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.util.EIMConfig;
import eim.util.StringUtils;
import eim.util.internal.search.ValueTypeEnum;
import jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl.DocumentIndexAdditionalInfoPlugInImpl.DocumentIndexAdditionalInfo;
import jp.co.ctc_g.eim.app.document.business.service.search.plugin.util.ContentSearchFieldDomainHelper;
import jp.co.ctc_g.eim.app.document.common.enumeration.search.ContentSearchFieldEnum;
import jp.co.ctc_g.eim.framework2.business.dao.AttributeTypeDao;
import jp.co.ctc_g.eim.framework2.business.dao.RelationDao;
import jp.co.ctc_g.eim.framework2.business.dao.RelationTypeDao;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.RelationTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionCompare;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionGroup;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchConditionIn;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchLimitCountCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectObject;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchSelectRelation;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;
import jp.co.ctc_g.eim.search.core.common.enumeration.FlagValueEnum;
import jp.co.ctc_g.eim.search.core.common.util.SearchConstant;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.FieldDomain;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.IndexDataDomain;
import jp.co.ctc_g.eim.search.core.indexBase.eim.business.service.plugin.util.FieldDomainHelper;
import jp.co.ctc_g.eim.search.core.searchApi.business.dao.SearchDao;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchRecordDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.SearchResultsDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.ValueFieldDomain;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.criteria.SearchConditionGroup.LogicalOperator;
import jp.co.ctc_g.eim.search.core.searchApi.business.domain.criteria.SearchCriteria;
import jp.co.ctc_g.eim.search.solr.common.enumeration.FileTypeEnum;
import jp.co.ctc_g.eim.search.solr.common.enumeration.ObjectSchemaFieldEnum;
import jp.co.ctc_g.eim.search.solr.indexBase.business.domain.SolrFieldDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.domain.SolrIndexDataDomain;
import jp.co.ctc_g.eim.search.solr.searchApi.business.domain.criteria.SolrSearchCriteria;

/**
 * ドキュメント情報の収集を行うプラグインクラスです。
 */
public class DocumentDataGatheringPlugInImpl extends BaseContentDataGatheringPlugInImpl {

	/** ロガー */
	private Log log = LogFactory.getLog(this.getClass());

	/** 検索Dao */
	private SearchDao searchDao = null;

	/** リレーションDao */
	private RelationDao relationDao = null;

	/** リレーションタイプDao */
	private RelationTypeDao relationTypeDao = null;

	/** 属性タイプDao */
	private AttributeTypeDao attributeTypeDao = null;

	/** 作成者属性の属性名 */
	protected final String CREATE_USER = EIMConfig.get("ATTR_NAME_DOCUMENT_CREATE");

	/** 公開処理失敗属性の属性名 */
	private final String PUB_PROC_FAIL = EIMConfig.get("ATTR_NAME_DOCUMENT_PUB_PROC_FAIL");

	/** PDF結合処理失敗属性の属性名 */
	private final String PDF_JOIN_FAIL = EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_JOIN_FAIL");

	/** 署名・暗号化状態属性の属性名 */
	private final String SIGN_ENC_STATUS = EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_STATUS");

	/** 改訂内容属性の属性名 */
	private final String REV_CONTENT = EIMConfig.get("ATTR_NAME_DOCUMENT_REV_CONTENT");

	/** 署名・暗号化バージョン属性の属性名 */
	private final String SIGN_ENC_VER = EIMConfig.get("ATTR_NAME_DOCUMENT_SIGN_ENC_VER");

	/** 上位WFフォルダ属性の属性名 */
	private final String HIGHER_WF_FOLDER = EIMConfig.get("ATTR_NAME_DOCUMENT_HIGHER_WF_FOLDER");

	/** リンク先属性の属性名 */
	private final String TO_LINK = EIMConfig.get("ATTR_NAME_DOCUMENT_TARGET_TO_LINK");

	/** PDF変換処理実行日時 */
	private final String PDF_CONV_EXEC_DATE = EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE");

	/** 公開PDF事前登録日時 */
	private final String PDF_PRE_REGIST_DATE = EIMConfig.get("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE");

	/** OCR処理ステータス */
	private final String OCR_PROC_STATUS = EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_PROCESS_STATUS");

	/** OCR結果ステータス */
	private final String OCR_RESULT_STATUS = EIMConfig.get("ATTR_NAME_DOCUMENT_OCR_RESULT_STATUS");

	/** 番号(自動採番) */
	private final String NUMBER = EIMConfig.get("ATTR_NAME_DOCUMENT_NUMBER");

	/** 帳票添付ファイルのオブジェクトタイプ名 */
	private final String OBJECT_TYPE_NAME_FORM_ATTACH_FILE = EIMConfig.get("OBJECT_TYPE_NAME_FORM_ATTACH_FILE");

	/** Solr検索時の結合条件(OR AND IN)の最大数 */
	private int searchConditionsLimit = 1000;

	/**
	 * ドキュメントの情報をインデックス登録情報ドメインに設定して返却します。<br>
	 * 「作成者」属性を更新します。
	 * さらに、ドキュメントリンクに対するインデックス登録情報ドメインを生成します。<br>
	 * 参照先のドキュメントのインデックス登録情報ドメインを複製し、リンクが配置されているフォルダの情報を元にパス、アクセス権限情報を更新します。
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.SolrEIMObjectDataPlugInImpl#getIndexDataList(java.util.List, java.util.List)
	 */
	@Override
	public List<IndexDataDomain> getIndexDataList(List<EIMObject> objectList, List<IndexDataDomain> indexDataList)
			throws Exception {

		// -------------------------------------
		// ドキュメントインデックスデータの生成
		// -------------------------------------

		// スーパークラスを呼び出しインデックスデータの生成
		List<IndexDataDomain> resultsIndexDataList = new ArrayList<>();

		List<IndexDataDomain> originalIndexDataList = super.getIndexDataList(objectList, indexDataList);
		Map<String, IndexDataDomain> idIndexDataMap =
				originalIndexDataList.stream().collect(Collectors.toMap(IndexDataDomain::getId, UnaryOperator.identity()));

		// 親のドキュメントリレーションが存在しない場合は、原本をインデックス作成対象から除去する

		// タスク管理の成果物は、親のドキュメントリレーションが存在しない。（親リレーションはリンクリレーションのみ）
		// ドキュメント管理の検索結果に表示したいのは成果物のリンクのみであるため。
		List<RelationDomain> parentRelationList = getParentDocumentRelation(objectList);
		if (!ObjectUtils.isEmpty(parentRelationList)) {
			Set<Long> childIdSet = parentRelationList.stream().map(RelationDomain::getChild).map(ObjectDomain::getId).collect(Collectors.toSet());

			for (IndexDataDomain originalIndexData : originalIndexDataList) {

				Long childId = Long.parseLong(originalIndexData.getId());
				if (!childIdSet.contains(childId)) {
					continue;
				}

				resultsIndexDataList.add(originalIndexData);
			}
		}

		// -------------------------------------
		// 作成者属性の上書き
		// -------------------------------------

		for (EIMObject obj : objectList) {

			// 作成者属性が設定されているか
			EIMAttribute createUserAttribute = obj.getAttribute(CREATE_USER);
			if (createUserAttribute == null) {
				continue;
			}

			// インデックス対象データ
			IndexDataDomain indexData = idIndexDataMap.get(String.valueOf(obj.getId()));

			// 設定済みのフィールドを削除
			indexData.getFieldList().removeIf(field -> field.getFieldName().equals(ObjectSchemaFieldEnum.CREATE_USER_JA_NAME.toString()));
			indexData.getFieldList().removeIf(field -> field.getFieldName().equals(ObjectSchemaFieldEnum.CREATE_USER_EN_NAME.toString()));

			// 作成者フィールドを追加
			FieldDomainHelper helper = new FieldDomainHelper();
			helper.addFieldListForCreateUser(createUserAttribute.getInt());
			// 多言語名称がnullの場合は定義名称をセット
			setOtherNameIfNull(helper.getFieldList() ,"CREATE_USER_JA_NAME" ,obj.getCreateUser().getDefName());
			setOtherNameIfNull(helper.getFieldList() ,"CREATE_USER_EN_NAME" ,obj.getCreateUser().getDefName());

			indexData.getFieldList().addAll(helper.getFieldList());
		}

		// ---------------------------------------
		// 登録済のインデックスデータを取得
		// ---------------------------------------

		// Solrの検索条件設定上限件数(デフォルト設定=solr.max.booleanClauses:1024)に準じて1000件毎に分割取得
		Map<String, List<SearchRecordDomain>> existingRecordListMap = new HashMap<>();
		List<EIMObject> devidedObjectList = new ArrayList<>();
		int count = 0;
		for (EIMObject object : objectList) {

			// 検索条件設定上限数に分割
			devidedObjectList.add(object);
			count ++;

			// 検索条件設定上限数か条件リストの終端まで繰り返し
			if (count % searchConditionsLimit != 0 && count != objectList.size()) {
				continue;
			}

			SearchCriteria criteria = new SolrSearchCriteria()
				.push(LogicalOperator.OR);
			// オブジェクトID
			for (EIMObject devidedObject : devidedObjectList) {
				String objId = String.valueOf(devidedObject.getId());
				criteria
					.addConditionString(ContentSearchFieldEnum.ID.toString(), objId);
			}
			criteria
				.pop();
			// 取得件数 (制限なし)
			criteria.setResultRows(Integer.MAX_VALUE);
			// 取得フィールド
			criteria.setResultFieldNameList(Arrays.asList(ContentSearchFieldEnum.ID.toString()));
			// アクセス権限を無視
			criteria.setIgnoreAuthorization(true);

			// 検索実行
			SearchResultsDomain results = searchDao.search(criteria);

			// 検索条件設定上限数毎にID:List<レコード>のMapに保管
			for (SearchRecordDomain record : results.getRecordList()) {
				ValueFieldDomain<String> idField = record.getValueField(ContentSearchFieldEnum.ID.toString());
				if (!existingRecordListMap.containsKey(idField.getValue())) {
					List<SearchRecordDomain> recordList = new ArrayList<>();
					existingRecordListMap.put(idField.getValue(), recordList);
				}
				existingRecordListMap.get(idField.getValue()).add(record);
			}

			devidedObjectList.clear();
		}

		// -------------------------------------------------
		// リンクリレーションからリンク更新タイミングを取得
		// -------------------------------------------------

		// 登録先フォルダの配下のオブジェクト取得
		SearchSelectObject.SearchConditionBuildHelper h = new SearchSelectObject.SearchConditionBuildHelper();
		SearchSelectRelation select = new SearchSelectRelation();
		SearchSelectObject parent = new SearchSelectObject();
		SearchSelectObject child = new SearchSelectObject();
		SearchLimitCountCondition limit = new SearchLimitCountCondition(SearchLimitCountCondition.UNLIMITED, false);

		// インデックス対象ドキュメントのID配列
		Set<Long> idSet = objectList.stream().map(obj -> (long) obj.getId()).collect(Collectors.toSet());
		long ids[] = idSet.stream().mapToLong(id -> Long.valueOf(id)).toArray();

		RelationTypeDomain relationType = relationTypeDao.getByDefinitionName(EIMConfig.getValue("RELATION_TYPE_NAME_LINK"));
		select.setCondition(
			h.group(
				h.opAnd()
				// 子がインデックス対象ドキュメント
				).addCondition(
					new SearchConditionIn(
						h.opAnd(),
						SearchSelectRelation.PsedoAttributeTypeEnum.CHILD,
						h.opIn(),
						ids
					)
				// リレーションタイプ「リンク」
				).addCondition(
					h.compare(h.opAnd(), SearchSelectRelation.PsedoAttributeTypeEnum.TYPE, h.opEq(), relationType.getId()
					)
				)
		);

		// 取得属性「リンク更新タイミング」
		AttributeTypeDomain linkUpdateTimingAttr = attributeTypeDao.getByDefinitionName(EIMConfig.getValue("ATTR_NAME_LINK_UPDATE_TIMING"));
		select.setResultAttrs(Arrays.asList(linkUpdateTimingAttr));
		parent.setResultAttrs(Collections.emptyList());
		child.setResultAttrs(Collections.emptyList());

		// リンクリレーション取得
		List<RelationDomain> linkRelationList = relationDao.getList(select, parent, child, limit);

		// 取得した「リンク更新タイミング」を保管
		Map<Long, Map<Long, Integer>> docIdToFolderIdToUpdateTiming = new HashMap<>();
		for (RelationDomain relation : linkRelationList) {
			AttributeDomain attrLinkUpdateTiming = relation.getAttribute(EIMConfig.getValue("ATTR_NAME_LINK_UPDATE_TIMING"));
			if (attrLinkUpdateTiming == null) {
				continue;
			}

			if (!docIdToFolderIdToUpdateTiming.containsKey(relation.getChild().getId())) {
				docIdToFolderIdToUpdateTiming.put(relation.getChild().getId(), new HashMap<>());
			}
			int linkUpdateTiming = (int) attrLinkUpdateTiming.getLong();
			docIdToFolderIdToUpdateTiming.get(relation.getChild().getId()).put(relation.getParent().getId(), linkUpdateTiming);
		}

		// -------------------------------------
		// リンクインデックスデータの生成
		// -------------------------------------

		// インデックス対象ドキュメント毎にリンクインデックスデータを生成
		for (EIMObject obj : objectList) {

			// パス属性
			EIMAttribute pathAttr = obj.getAttribute(PATH);
			if (pathAttr == null) {
				// オブジェクトのパス属性が取得できません。
				log.warn(ResourceUtils.getByKey("EIM.ERROR.LOGIC.NO.PATH.ATTRIBUTE", obj.getId()));
				continue;
			}

			// インデックス対象データ
			IndexDataDomain indexData = idIndexDataMap.get(String.valueOf(obj.getId()));

			// 既存データが存在して、リンクが増えている場合はファイルおよび添付ファイルの全文データを取得
			List<SearchRecordDomain> existingRecordList = existingRecordListMap.get(indexData.getId());
			String fullText = null;
			List<Long> attachmentIdList = null;
			List<String> attachmentFullTextList = null;
			if (existingRecordList != null && existingRecordList.size() > 0 && existingRecordList.size() < pathAttr.getStrings().length) {
				// キーを指定してファイルおよび添付ファイルの全文データを取得
				ValueFieldDomain<String> keyField = existingRecordList.get(0).getValueField(ContentSearchFieldEnum.KEY.toString());
				SearchCriteria fullTextCriteria = new SolrSearchCriteria().addConditionString(ContentSearchFieldEnum.KEY.toString(), keyField.getValue());
				// 取得件数
				fullTextCriteria.setResultRows(1);
				// 取得フィールド
				fullTextCriteria.setResultFieldNameList(Arrays.asList(
						ContentSearchFieldEnum.DOC_FULL_TEXT.toString(),
						ContentSearchFieldEnum.ATTACHMENT_IDS.toString(),
						ContentSearchFieldEnum.ATTACHMENT_FULL_TEXTS.toString()));
				// アクセス権限を無視
				fullTextCriteria.setIgnoreAuthorization(true);

				// 検索実行
				SearchResultsDomain fullTextResult = searchDao.search(fullTextCriteria);

				// 結果データ取得
				if (fullTextResult.getRecordList() != null && fullTextResult.getRecordList().size() > 0) {

					SearchRecordDomain fullTextRecord = fullTextResult.getRecordList().get(0);

					// ファイル全文
					ValueFieldDomain<String> fullTextField = fullTextRecord.getValueField(ContentSearchFieldEnum.DOC_FULL_TEXT.toString());
					if (fullTextField != null) {
						fullText = fullTextField.getValue();
					}

					// 添付ファイルID
					ValueFieldDomain<List<Long>> attachmentIdsField = fullTextRecord.getValueField(ContentSearchFieldEnum.ATTACHMENT_IDS.toString());
					if (attachmentIdsField != null) {
						attachmentIdList = attachmentIdsField.getValue();
					}

					// 添付ファイル全文
					ValueFieldDomain<List<String>> attachmentFullTextsField = fullTextRecord.getValueField(ContentSearchFieldEnum.ATTACHMENT_IDS.toString());
					if (attachmentFullTextsField != null) {
						attachmentFullTextList = attachmentFullTextsField.getValue();
					}
				}
			}

			// 既存データが存在して、リンクが減っている場合は登録済のリンクデータを削除
			if (existingRecordList != null && existingRecordList.size() > pathAttr.getStrings().length) {
				for (int i = pathAttr.getStrings().length; i < existingRecordList.size(); i ++) {
					// インデックスデータを複製、更新種別を削除にして追加
					SolrIndexDataDomain copyIndexData = new SolrIndexDataDomain(indexData);
					copyIndexData.setSeq(i);
					copyIndexData.setUpdateKind(SearchConstant.UPDATECODE_INDEX_DEL);
					resultsIndexDataList.add(copyIndexData);
				}
			}

			// リンク先属性(リンクの親フォルダID)
			EIMAttribute toLinkAttr = obj.getAttribute(TO_LINK);

			// リンク先属性(リンクの親フォルダID)が存在しない場合
			if (toLinkAttr == null || toLinkAttr.getInts() == null) {
				continue;
			}

			// リンク先属性(リンクの親フォルダID)の数だけインデックスデータを複製してリンクデータを生成
			int seq = 1;
			for (long parentId : toLinkAttr.getInts()) {

				// 不正データを回避するためリンクの存在チェック
				if (!docIdToFolderIdToUpdateTiming.containsKey((long) obj.getId()) ||
						!docIdToFolderIdToUpdateTiming.get((long) obj.getId()).containsKey((long) parentId)) {
					log.warn(ResourceUtils.getByKey("EIM.ERROR.LOGIC.LINK.RELATION.NOTFOUND", obj.getId(), parentId));
					continue;
				}

				// Solr用インデックスデータをコピー生成して返却リストに追加
				SolrIndexDataDomain linkIndexData = new SolrIndexDataDomain(indexData);
				linkIndexData.setSeq(seq);
				resultsIndexDataList.add(linkIndexData);

				// リンクの場合、IDに「ID_親フォルダID」を設定する
				// ※ 後続のアクセス権限情報設定処理で親フォルダのアクセス権限情報を設定するため
				linkIndexData.setId(String.valueOf(linkIndexData.getId() + "_" + parentId));

				// ドキュメントのIDを一時的にフィールド値として保持
				// ※ インデックス更新サービスでフォルダIDをドキュメントIDに書き換える
				FieldDomain idField = new FieldDomain(ContentSearchFieldEnum.ID.toString(), indexData.getId());
				linkIndexData.getFieldList().add(idField);

				// リンクの親フォルダIDを設定
				FieldDomain linkParentField = new FieldDomain(ContentSearchFieldEnum.LINK_PARENT_ID.toString(), parentId);
				linkIndexData.getFieldList().add(linkParentField);

				// リンク更新タイミングの設定
				int linkUpdateTiming = docIdToFolderIdToUpdateTiming.get((long) obj.getId()).get((long) parentId);
				FieldDomain linkUpdateTimingField = new FieldDomain(ContentSearchFieldEnum.LINK_UPDATE_TIMING.toString(), (long) linkUpdateTiming);
				linkIndexData.getFieldList().add(linkUpdateTimingField);

				// パスフィールドを取得
				FieldDomain pathField = linkIndexData.getField(ContentSearchFieldEnum.PATH.toString());

				// パスフィールドを削除
				int pathFieldIndex = linkIndexData.getFieldList().indexOf(pathField);
				linkIndexData.getFieldList().remove(pathField);

				// パスフィールドを設定
				String path = pathAttr.getStrings()[seq];
				FieldDomain newPathField = new FieldDomain(ContentSearchFieldEnum.PATH.toString(), path);
				linkIndexData.getFieldList().add(pathFieldIndex, newPathField);

				// 追加登録するリンクデータに登録済のファイル全文、添付ファイル全文を設定する
				if (existingRecordList != null  && existingRecordList.size() < seq + 1) {
					// ファイル全文
					if (fullText != null) {
						FieldDomain fullTextField = new SolrFieldDomain(ContentSearchFieldEnum.DOC_FULL_TEXT.toString(), fullText);
						linkIndexData.getFieldList().add(fullTextField);
					}

					// 添付ファイルID
					if (attachmentIdList != null) {
						Long[] attachmentIds = attachmentIdList.toArray(new Long[attachmentIdList.size()]);
						FieldDomain fullTextField = new SolrFieldDomain(ContentSearchFieldEnum.ATTACHMENT_IDS.toString(), attachmentIds);
						linkIndexData.getFieldList().add(fullTextField);
					}

					// 添付ファイル全文
					if (attachmentFullTextList != null) {
						Long[] attachmentFullTexts = attachmentFullTextList.toArray(new Long[attachmentFullTextList.size()]);
						FieldDomain fullTextField = new SolrFieldDomain(ContentSearchFieldEnum.ATTACHMENT_FULL_TEXTS.toString(), attachmentFullTexts);
						linkIndexData.getFieldList().add(fullTextField);
					}
				}

				seq ++;
			}
		}

		return resultsIndexDataList;
	}

	/**
	 * 多言語名称が空の場合に、定義名称をセットします。（作成者、更新者など）
	 * @param fieldList フィールドリスト
	 * @param fieldName フィールド名称
	 * @param defName 定義名称
	 */
	private void setOtherNameIfNull(List<FieldDomain> fieldList, String fieldName, String defName) {
		FieldDomain field = fieldList.stream()
								.filter(f -> f.getFieldName().equals(fieldName))
								.findFirst().get();

		if (field.getStringValue() == null) {
			field.setStringValue(defName);
		}
	}

	/**
	 * ドキュメント固有のシステム属性タイプの情報を元にフィールド情報ドメインを生成して返却します。
	 * @see jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl.BaseContentDataGatheringPlugInImpl#getAdditionalFieldList(eim.bo.EIMObject, java.lang.Object)
	 */
	@Override
	protected List<FieldDomain> getAdditionalFieldList(EIMObject obj, Object additionalInfo) throws Exception {

		// 共用情報は一括取得済みのファイル情報とステータスなし公開ドキュメント
		DocumentIndexAdditionalInfo documentAdditionalInfo = (DocumentIndexAdditionalInfo) additionalInfo;
		Map<Long, FileDomain> publicFileMap = documentAdditionalInfo.getFileMapByFormatName(EIMConfig.get("FORMAT_NAME_PUBLIC"));
		Set<Long> noSTPublicObjSet = documentAdditionalInfo.getNoStatusPublicObjectSet();

		ContentSearchFieldDomainHelper fieldHelper = new ContentSearchFieldDomainHelper();

		// 公開処理失敗
		EIMAttribute attPublicProcFailure = obj.getAttribute(PUB_PROC_FAIL);
		if(attPublicProcFailure != null){
			fieldHelper.addFieldForPublicProcFailure(attPublicProcFailure.getInt());
		}

		// PDF結合処理失敗
		EIMAttribute attFailedToPdfMerge = obj.getAttribute(PDF_JOIN_FAIL);
		if(attFailedToPdfMerge != null){
			fieldHelper.addFieldForFailedToPdfMerge(attFailedToPdfMerge.getInt());
		}

		// 署名・暗号化状態
		EIMAttribute attSignencr = obj.getAttribute(SIGN_ENC_STATUS);
		if(attSignencr != null){
			fieldHelper.addFieldForSignencr(attSignencr.getInt());
		}

		// 改訂内容
		EIMAttribute attRevisedContent = obj.getAttribute(REV_CONTENT);
		if(attRevisedContent != null){
			fieldHelper.addFieldForRevisedContent(attRevisedContent.getString());
		}

		// 署名・暗号化バージョン
		EIMAttribute attSignatureAndEncryptionVer = obj.getAttribute(SIGN_ENC_VER);
		if(attSignatureAndEncryptionVer != null){
			fieldHelper.addFieldForSignatureAndEncryptionVer(attSignatureAndEncryptionVer.getString());
		}

		// ワークフロー付きフォルダID
		EIMAttribute attWfAttachedFolder = obj.getAttribute(HIGHER_WF_FOLDER);
		if(attWfAttachedFolder != null){
			fieldHelper.addFieldForWfAttachedFolderId(attWfAttachedFolder.getInt());
		}

		// PDF変換処理実行日時
		EIMAttribute attPdfConvExecDate = obj.getAttribute(PDF_CONV_EXEC_DATE);
		if(attPdfConvExecDate != null){
			fieldHelper.addField(ContentSearchFieldEnum.PDF_CONV_EXEC_DATE.toString(), attPdfConvExecDate.getDate());
		}

		// 公開PDF事前登録日時
		EIMAttribute attPdfPreRegistDate = obj.getAttribute(PDF_PRE_REGIST_DATE);
		if(attPdfPreRegistDate != null){
			fieldHelper.addField(ContentSearchFieldEnum.PDF_PRE_REGIST_DATE.toString(), attPdfPreRegistDate.getDate());
		}

		// OCR処理ステータス
		EIMAttribute attOcrProcStatus = obj.getAttribute(OCR_PROC_STATUS);
		if(attOcrProcStatus != null){
			fieldHelper.addField(ContentSearchFieldEnum.OCR_PROC_STATUS.toString(), (long) attOcrProcStatus.getInt());
		}

		// OCR結果ステータス
		EIMAttribute attOcrResultStatus = obj.getAttribute(OCR_RESULT_STATUS);
		if(attOcrResultStatus != null){
			fieldHelper.addField(ContentSearchFieldEnum.OCR_RESULT_STATUS.toString(), (long) attOcrResultStatus.getInt());
		}

		// 番号(自動採番)
		EIMAttribute attNumber = obj.getAttribute(NUMBER);
		if(attNumber != null){
			fieldHelper.addField(ContentSearchFieldEnum.AUTO_NUMBER.toString(), attNumber.getString());
		}

		// 公開 (有効な公開ファイルを保持している場合にtrueとする)
		FileDomain publicFile = null;
		if (publicFileMap != null) {
			publicFile = publicFileMap.get(Long.valueOf(obj.getId()));
		}
		if (publicFile != null) {
			String pubExt = publicFile.getExt();
			if(!StringUtils.isBlank(pubExt)) {
				// 「.」を除外
				pubExt = pubExt.substring(1, pubExt.length());
			}
			// 公開ファイルが存在する場合
			// TODO 改訂直後のWFなしPDFの公開アイコンが表示されてしまう(既存バグ？)拡張子ではなく変換ステータスで判定すべきか？
			if (obj.getStatus() != null && obj.getStatus().getType().getKind() == AppConstant.STATUS_TYPE_KIND_ID_PUBLIC
					|| (pubExt != null && pubExt.equalsIgnoreCase(FileTypeEnum.PDF.getValue()))
					|| noSTPublicObjSet.contains((long) obj.getId())) {
				// ステータスが公開済み
				// or PDF変換処理によって公開PDFが生成されている
				// or 公開ワークフローなしドキュメントに該当する (改訂中の場合は非公開扱い)
				fieldHelper.addFieldForPublic(FlagValueEnum.ON.getValue());
			} else {
				fieldHelper.addFieldForPublic(FlagValueEnum.OFF.getValue());
			}
		} else {
			fieldHelper.addFieldForPublic(FlagValueEnum.OFF.getValue());
		}

		// 添付ファイル
		@SuppressWarnings("unchecked")
		List<EIMAttribute> attrList = obj.getAttributeList();

		// 添付ファイル属性の抽出
		List<EIMAttribute> attachmentAttrList = attrList.stream()
				// オブジェクト型
				.filter(attr -> attr.getType().getValueType().getId() == ValueTypeEnum.OBJECT.getId())
				// 複数値
				.filter(attr -> attr.getType().isMultiple() == true)
				// オブジェクトのクラスが「app.form.dev:帳票添付ファイル」
				.filter(attr -> attr.getObjects()[0].getType().getDefName().equals(OBJECT_TYPE_NAME_FORM_ATTACH_FILE))
				.collect(Collectors.toList());

		// 添付ファイル名を取得
		List<String> attachmentNameList = new ArrayList<>();
		for (EIMAttribute attachmentAttr : attachmentAttrList) {
			for (EIMObject attachment : attachmentAttr.getObjects()) {
				attachmentNameList.add(attachment.getName());
			}
		}

		// 添付ファイル名
		if (attachmentNameList.size() > 0) {
			String[] attachmentNames = attachmentNameList.toArray(new String[attachmentNameList.size()]);
			fieldHelper.addField(ContentSearchFieldEnum.ATTACHMENT_NAMES.toString(), attachmentNames);
		}

		// フィールドリストを取得
		List<FieldDomain> fieldList = fieldHelper.getFieldList();

		// 拡張属性をフィールドリストに追加
		fieldList.addAll(super.getAdditionalFieldList(obj, documentAdditionalInfo));

		return fieldList;
	}

	/**
	 * 親のドキュメントリレーションリストを取得します。
	 *
	 * @param childList 子オブジェクトのリスト
	 * @return 親のドキュメントリレーションリスト
	 */
	private List<RelationDomain> getParentDocumentRelation(List<EIMObject> childList) throws Exception {

		// ドキュメントリレーション存在チェック
		SearchSelectRelation selectRelation = new SearchSelectRelation();
		SearchConditionGroup conditions = new SearchConditionGroup(SearchOperatorEnum.AND);
		selectRelation.setCondition(conditions);
		SearchSelectObject selectParent = new SearchSelectObject();
		SearchSelectObject selectChild = new SearchSelectObject();

		// リレーションタイプ
		RelationTypeDomain relationType = relationTypeDao.getByDefinitionName(ConfigUtils.getByKey("RELATION_TYPE_NAME_DOCUMENT"));
		conditions.addCondition(
				new SearchConditionCompare(
						SearchOperatorEnum.AND,
						SearchSelectRelation.PsedoAttributeTypeEnum.TYPE,
						SearchOperatorEnum.EQ,
						relationType.getId()
						)
				);

		// 子オブジェクト
		long[] childIds = childList.stream().mapToLong(o -> o.getId()).toArray();
		conditions.addCondition(
				new SearchConditionIn(
						SearchOperatorEnum.AND,
						SearchSelectRelation.PsedoAttributeTypeEnum.CHILD,
						SearchOperatorEnum.IN,
						childIds
						)
				);

		// 取得条件
		selectRelation.setResultAttrs(new ArrayList<>());
		SearchLimitCountCondition limitCond = new SearchLimitCountCondition(SearchLimitCountCondition.UNLIMITED, false);

		// リレーション検索
		List<RelationDomain> relationList = relationDao.getList(selectRelation, selectParent, selectChild, limitCond);

		return relationList;
	}

	/**
	 * 検索Daoを取得します。
	 * @return 検索Dao
	 */
	public SearchDao getSearchDao() {
		return searchDao;
	}

	/**
	 * 検索Daoを設定します。
	 * @param searchDao 検索Dao
	 */
	public void setSearchDao(SearchDao searchDao) {
		this.searchDao = searchDao;
	}

	/**
	 * リレーションDaoを取得します。
	 * @return リレーションDao
	 */
	public RelationDao getRelationDao() {
		return relationDao;
	}

	/**
	 * リレーションDaoを設定します。
	 * @param relationDao リレーションDao
	 */
	public void setRelationDao(RelationDao relationDao) {
		this.relationDao = relationDao;
	}

	/**
	 * リレーションタイプDaoを取得します。
	 * @return リレーションタイプDao
	 */
	public RelationTypeDao getRelationTypeDao() {
		return relationTypeDao;
	}

	/**
	 * リレーションタイプDaoを設定します。
	 * @param relationTypeDao リレーションタイプDao
	 */
	public void setRelationTypeDao(RelationTypeDao relationTypeDao) {
		this.relationTypeDao = relationTypeDao;
	}

	/**
	 * 属性タイプDaoを取得します。
	 * @return 属性タイプDao
	 */
	public AttributeTypeDao getAttributeTypeDao() {
		return attributeTypeDao;
	}

	/**
	 * 属性タイプDaoを設定します。
	 * @param attributeTypeDao 属性タイプDao
	 */
	public void setAttributeTypeDao(AttributeTypeDao attributeTypeDao) {
		this.attributeTypeDao = attributeTypeDao;
	}

	/**
	 * Solr検索時の結合条件(OR AND IN)の最大数を取得します。
	 * @return Solr検索時の結合条件(OR AND IN)の最大数
	 */
	public int getSearchConditionsLimit() {
		return searchConditionsLimit;
	}

	/**
	 * Solr検索時の結合条件(OR AND IN)の最大数を設定します。
	 * @param searchConditionLimit Solr検索時の結合条件(OR AND IN)の最大数
	 */
	public void setSearchConditionsLimit(int searchConditionsLimit) {
		this.searchConditionsLimit = searchConditionsLimit;
	}

}
