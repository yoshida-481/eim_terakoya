package jp.co.ctc_g.eim.app.document.business.service.search.impl;

import java.util.List;

import jp.co.ctc_g.eim.app.document.common.enumeration.search.ContentSearchFieldEnum;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.FieldDomain;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.IndexDataDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.domain.SolrIndexDataDomain;
import jp.co.ctc_g.eim.search.solr.indexBase.business.service.impl.SolrAttributesIndexUpdateServiceImpl;

/**
 * 検索インデックス更新処理を行うサービス実装のドキュメント管理用の拡張クラスです。<p>
 * ドキュメントリンクへの対応が実装されています。
 */
public class ContentAttributesIndexUpdateServiceImpl extends SolrAttributesIndexUpdateServiceImpl {

	/**
	 * 検索インデックスデータを更新します。<p>
	 * ドキュメントリンクインデックスデータのIDを親フォルダIDからドキュメントIDに書き換えます。<br>
	 * ※ドキュメントリンクのアクセス権限情報に親フォルダの権限情報をマージ設定する必要があり、一時的に親フォルダIDが設定されているため
	 * @see jp.co.ctc_g.eim.search.solr.indexBase.business.service.impl.SolrAttributesIndexUpdateServiceImpl#update(java.util.List)
	 */
	@Override
	public void update(List<IndexDataDomain> indexDataList) throws Exception {

		// ドキュメントリンクインデックスデータのIDを親フォルダIDからドキュメントIDに書き換える
		for (IndexDataDomain indexDataDomain : indexDataList) {

			// Solr用のインデックスデータにキャスト
			SolrIndexDataDomain indexData = (SolrIndexDataDomain) indexDataDomain;

			// リンクはシーケンスが1以上
			if (indexData.getSeq() > 0) {

				// IDフィールドからドキュメントIDを取得し、インデックスデータのインスタンス変数を上書き
				FieldDomain idField = indexData.getField(ContentSearchFieldEnum.ID.toString());
				indexData.setId(idField.getStringValue());
			}
		}

		// インデックス更新処理の実行
		super.update(indexDataList);
	}

}
