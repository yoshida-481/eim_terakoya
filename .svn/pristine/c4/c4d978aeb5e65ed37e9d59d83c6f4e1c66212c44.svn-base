package jp.co.ctc_g.eim.app.document.business.service.search.plugin.impl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import eim.bo.EIMAttribute;
import eim.bo.EIMObject;
import eim.util.EIMConfig;
import jp.co.ctc_g.eim.framework2.business.domain.entity.FileDomain;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.IndexDataDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.domain.SolrIndexDataDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.FileIndexAdditionalInfoPlugInImpl.FileIndexAdditionalInfo;
import jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.SolrEIMFileDataPlugInImpl;

/**
 * ドキュメントが保持するファイル情報の収集を行うプラグインクラスです。
 */
public class DocumentFileDataGatheringPlugInImpl extends SolrEIMFileDataPlugInImpl {

	/** パス属性の属性名 */
	protected final String PATH = EIMConfig.get("ATTR_NAME_DOCUMENT_PASS");

	/**
	 * インデックス登録情報ドメインを生成して返却します。<br>
	 * パス属性の数分 (原本+リンク数)のインデックス登録情報ドメインが生成されます。<br>
	 * シーケンス番号は、原本が0、リンクが1以降となります。
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.SolrEIMFileDataPlugInImpl#getIndexDataList(eim.bo.EIMObject, jp.co.ctc_g.eim.search.core.indexBase.business.domain.IndexDataDomain)
	 */
	@Override
	protected List<SolrIndexDataDomain> getIndexDataList(EIMObject object, IndexDataDomain indexData) {

		// 返却するSolr用インデックスデータリスト
		List<SolrIndexDataDomain> resultIndexDataList = new ArrayList<>();

		// パス属性の取得
		EIMAttribute pathAttr = object.getAttribute(PATH);
		int length = 1;
		if (pathAttr != null && pathAttr.getStrings() != null) {
			length = pathAttr.getStrings().length;
		}

		// パス属性の数分 (原本+リンク数)ループしてドキュメントリンクに対してもインデックスデータを生成
		for (int seq = 0; seq < length; seq ++) {
			// 入力されたインデックスデータを元にSolr用インデックスデータを生成
			SolrIndexDataDomain newIndexData = new SolrIndexDataDomain(indexData);
			resultIndexDataList.add(newIndexData);

			// 同一IDに対するシーケンス番号の設定
			newIndexData.setSeq(seq);
		}

		return resultIndexDataList;
	}

	/**
	 * 原本または公開ファイルの情報を取得して返却します。<br>
	 * 原本ファイルよりも新しい公開ファイルがある場合はそれを優先します。それ以外は原本ファイルのパスを取得します。
	 * @throws Exception
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.SolrEIMFileDataPlugInImpl#getTargetFile(eim.bo.EIMObject, jp.co.ctc_g.eim.search.solr.indexBase.business.service.plugin.impl.FileIndexAdditionalInfoPlugInImpl.FileIndexAdditionalInfo)
	 */
	@Override
	protected FileDomain getTargetFile(EIMObject object, FileIndexAdditionalInfo additionalInfo) throws Exception {

		// 一括取得済みのファイル情報
		Map<Long, FileDomain> defaultFileMap = additionalInfo.getDefaultFileMap();
		Map<Long, FileDomain> publicFileMap = additionalInfo.getFileMapByFormatName(EIMConfig.get("FORMAT_NAME_PUBLIC"));

		// PDF変換処理実行日時、または、公開PDF事前登録日時 が更新日時より大きい場合公開ファイル情報を取得(ページ検索のために公開ファイルを優先する)
		FileDomain file = null;
		Date modifyDate = object.getModifyDate();
		EIMAttribute attrPdfConvExecDate = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_CONV_EXEC_DATE"));
		if (publicFileMap != null && attrPdfConvExecDate != null && attrPdfConvExecDate.getDate().getTime() > modifyDate.getTime()) {
			file = publicFileMap.get(Long.valueOf(object.getId()));
		}
		EIMAttribute attrPdfPreRegistDate = object.getAttribute(ConfigUtils.getByKey("ATTR_NAME_DOCUMENT_PDF_PRE_REGIST_DATE"));
		if (publicFileMap != null && attrPdfPreRegistDate != null && attrPdfPreRegistDate.getDate().getTime() > modifyDate.getTime()) {
			file = publicFileMap.get(Long.valueOf(object.getId()));
		}
		if (file == null) {
			// 公開ファイルが無い場合は原本ファイルを取得
			file = defaultFileMap.get(Long.valueOf(object.getId()));
		}

		return file;
	}

}
