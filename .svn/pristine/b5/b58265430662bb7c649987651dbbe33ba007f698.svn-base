package jp.co.ctc_g.eim.app.document.business.service.search.plugin.util;

import java.util.Date;

import jp.co.ctc_g.eim.search.app.document.business.service.plugin.util.FieldDomainHelper;
import jp.co.ctc_g.eim.search.core.indexBase.business.domain.FieldDomain;

/**
 * インデックスフィールドデータ設定ヘルパークラスのドキュメント管理アプリケーション用拡張クラスです。
 */
public class ContentSearchFieldDomainHelper extends FieldDomainHelper {

	/**
	 * インデックスフィールドデータを追加します。
	 * @param fieldName
	 * @param value
	 */
	public <T> void addField(String fieldName, T value) {

		// 入力値の型に応じてFieldDomainを生成 (SolrFieldDomainでは実数型なども対応するが呼出し元で未使用なのでFieldDomainの型に限定)
		FieldDomain fieldDomain = null;
		if (value instanceof Long) {
			fieldDomain = new FieldDomain(fieldName, (Long) value);
		} else if (value instanceof Date) {
			fieldDomain = new FieldDomain(fieldName, (Date) value);
		} else if (value instanceof String) {
			fieldDomain = new FieldDomain(fieldName, (String) value);
		} else if (value instanceof String[]) {
			fieldDomain = new FieldDomain(fieldName, (String[]) value);
		}

		// 基底クラスのフィールドリストに追加
		getFieldList().add(fieldDomain);
	}

}
