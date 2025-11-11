package jp.co.ctc_g.eim.app.document.business.plugin.box.impl;

import java.text.SimpleDateFormat;
import java.util.Date;

import jp.co.ctc_g.eim.framework2.business.domain.criteria.FileCriteria.FileItemEnum;
import jp.co.ctc_g.eim.framework2.business.domain.criteria.FileCriteria.SubQueryCondition;
import jp.co.ctc_g.eim.framework2.business.domain.search.SearchOperatorEnum;
import jp.co.ctc_g.eim.framework2.business.plugin.box.impl.ElapsedDaysArchiveConditionPlugInImpl;

/**
 * Boxへのファイル保管の対象となるオブジェクトの抽出条件を設定するプラグインクラスです。
 * 指定日数内のダウンロード回数が指定回数（設定ファイルの「最低ダウンロード回数」）を上回って回いるものを除外する条件を指定します。
 */
public class DownloadTimesArchiveConditionPlugInImpl extends ElapsedDaysArchiveConditionPlugInImpl {

	/** 最低ダウンロード回数 */
	private int minDownloadNum = Integer.MAX_VALUE;

	/**
	 * @see jp.co.ctc_g.eim.framework2.business.plugin.box.ArchiveConditionPlugIn#getCondition()
	 */
	@Override
	public SubQueryCondition getCondition() {

		// 境界値として(現在日時 - 最大経過日数)日の午前0時を取得する
		Date boundaryDate = getBoundaryDate();
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");

		// サブクエリーを設定する
		String query =
				"SELECT " +
						"oid " +
					"FROM " +
						"( " +
							"SELECT " +
									"COUNT(ACCS.oid) COUNT " +
									",ACCS.oid " +
								"FROM " +
									"eimaccs ACCS " +
								"WHERE " +
									"ACCS.adate >= TO_DATE('%s', 'yyyy-mm-dd') " +
									"AND ACCS.action = 'EIM.ACCESS.TYPE.DOWNLOAD' " +
								"GROUP BY " +
									"ACCS.oid " +
						") DLCNT " +
					"WHERE " +
						"COUNT >= %d";

		// 条件値を置換する
		String formatedQuery = String.format(query, sdf.format(boundaryDate), minDownloadNum);

		// カラム[OBJECT]を[NOT IN]で検索する
		SubQueryCondition condition = new SubQueryCondition(FileItemEnum.OBJECT, SearchOperatorEnum.NOT_IN, formatedQuery);

		return condition;
	}

	/**
	 * 最低ダウンロード回数を設定します。
	 * @param minDownloadNum 最低ダウンロード回数
	 */
	public void setMinDownloadNum(int minDownloadNum) {
		this.minDownloadNum = minDownloadNum;
	}
}
