package jp.co.ctc_g.eim.app.document.business.plugin.box.impl;

import jp.co.ctc_g.eim.framework2.business.domain.criteria.FileCriteria.FileItemEnum;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.FileCriteria.SubQueryCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.plugin.box.ArchiveConditionPlugIn;

/**
 * Boxへのファイル保管の対象となるオブジェクトの抽出条件を設定するプラグインクラスです。
 * PDF変換指示オブジェクトが存在するものを除外する条件を指定します。
 */
public class PDFConvertionArchiveConditionPlugInImpl implements ArchiveConditionPlugIn {

	/** PDF変換オブジェクトタイプ名 */
	private String objectTypeNamePDFConv = null;

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.plugin.box.ArchiveConditionPlugIn#getCondition()
	 */
	@Override
	public SubQueryCondition getCondition() {
		// サブクエリーを設定する
		String query =
				"SELECT " +
						"CAST(OBJ.name as NUMERIC(32,0)) " +
					"FROM " +
						"eimobj OBJ " +
						",eimobjtype OTYPE " +
					"WHERE " +
						"OBJ.type = OTYPE.id " +
						"AND OTYPE.name = '%s'";

		// 条件値を置換する
		String formatedQuery = String.format(query, objectTypeNamePDFConv);

		// カラム[OBJECT]を[NOT_IN]で検索する
		SubQueryCondition condition = new SubQueryCondition(FileItemEnum.OBJECT, SearchOperatorEnum.NOT_IN, formatedQuery);

		return condition;
	}

	/**
	 * PDF変換オブジェクトタイプ名を設定します。
	 * @param objectTypeNamePDFConv PDF変換オブジェクトタイプ名
	 */
	public void setObjectTypeNamePDFConv(String objectTypeNamePDFConv) {
		this.objectTypeNamePDFConv = objectTypeNamePDFConv;
	}

}
