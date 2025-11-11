package common.util;

import java.util.List;

import eim.util.internal.search.sql.SearchSqlUtil;

/**
 * SQLを扱うUtilクラスです。
 */
public class AppSqlUtil {

	/**
	 * ステータスなし公開　過去レビジョンを全て取得
	 *
	 */
	private static final String NOSTATAS_PUBLIC_OBJSQL_FOR_PASTDOC =
			" select eimobj.id as OID"+
				" from " +
					" eimobj, eimver, " +
					" (select oob.id as LID,vo.vid, oob.rev " +
						" from " +
							" eimobj oob, " +
							" eimver vo, " +
							" (select eimver.vid ,max(rev) as rev " +
								" from " +
									" eimobj iob, " +
									" eimver " +
								" where " +
									" eimver.oid = iob.id " +
									" and vid in (select vid from eimver where %1$s) " +	// 条件式を挿入
								" group by eimver.vid " +
							" ) vv " +
						" where " +
							" oob.id = vo.oid and vv.vid = vo.vid " +
							" and oob.rev = vv.rev " +
							" and oob.rev > 0 " +
					" ) LO " +
					" where " +
						" eimobj.id = eimver.oid " +
						" and LO.rev - 1 >= eimobj.rev " + // 過去のレビジョン全て
						" and eimver.vid = Lo.vid ";

	/**
	 * ステータスなし公開
	 *
	 * <li>1つ目のselect 改定なし (最新をチェックイン済みの状態の最新のドキュメントを取得) <li>2つのselect 初期登録
	 * (0版で最新版のドキュメントを取得)
	 */
	private static final String NOSTATAS_PUBLIC_OBJSQL_UNION_FOR_LATESTDOC_AND_INITDOC =
			" union " +
			" select eimobj.id as OID" +
				" from " +
					" eimobj, eimver, " +
					" (select oob.id as LID,vo.vid, oob.rev " +
						" from " +
							" eimobj oob, " +
							" eimver vo, " +
							" (select eimver.vid ,max(rev)-1 as rev " +
								" from " +
									" eimobj iob, " +
									" eimver " +
								" where " +
									" eimver.oid = iob.id " +
									" and vid in (select vid from eimver where %1$s) " +	// 条件式を挿入
								" group by eimver.vid " +
							" ) vv " +
						" where " +
							" oob.id = vo.oid and vv.vid = vo.vid " +
							" and oob.rev = vv.rev " +
							" and oob.latest = 0 " +
					" ) LB " +
				" where " +
					" eimobj.id = eimver.oid " +
					" and LB.rev + 1 = eimobj.rev " +
					" and eimobj.latest = 1 " +
					" and eimver.vid = LB.vid " +
			" union " +
			" select id " +
				" from eimobj " +
				" where " +
					" rev = 0 " +
					" and latest = 1 " +
					" and luser is null " +
					" and status = 0 ";

	/**
	 * (公開アイコン表示判定用) ステータスなし公開
	 *
	 * <li>1つ目のselect 改定中 過去レビジョンを全て取得 <li>2つ目のselect 改定なし <li>3つのselect 初期登録
	 */
	private static final String NOSTATAS_PUBLIC_ICON_OBJSQL =
			" select OID " +
				" from "+
					" ("+
						NOSTATAS_PUBLIC_OBJSQL_FOR_PASTDOC +
						NOSTATAS_PUBLIC_OBJSQL_UNION_FOR_LATESTDOC_AND_INITDOC +
					" ) sub " +
				" where %1$s";	// 条件式を挿入

	/**
	 * (公開アイコン表示判定用) ステータスなし公開オブジェクトを取得するクエリー文字列を返却します。
	 * <li>1つ目のselect 改定中 過去レビジョンを全て取得 <li>2つ目のselect 改定なし <li>3つのselect 初期登録
	 * @param objectIds 対象のオブジェクトIDリスト
	 * @return (公開アイコン表示判定用) ステータスなし公開オブジェクトを取得するクエリー文字列
	 */
	public static String getSqlNoStatusPublicIconObj(List<Long> objectIds) {
		// クエリー文字列定数
		String baseQueryString = NOSTATAS_PUBLIC_ICON_OBJSQL;

		// 対象のオブジェクトIDリストを指定するin句を生成
		StringBuffer conditionSb = new StringBuffer();
		SearchSqlUtil.expandListToIn(conditionSb, objectIds, "OID");

		// クエリー文字列定数に含まれる書式指定文字列をin句で置換
		String quryString = String.format(baseQueryString, conditionSb.toString());

		return quryString;
	}
}
