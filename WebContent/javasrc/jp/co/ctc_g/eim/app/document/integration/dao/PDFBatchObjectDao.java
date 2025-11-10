package jp.co.ctc_g.eim.app.document.integration.dao;

/**
 * 排他ロックに関するObjectDaoの追加機能インターフェイスです。
 */
public interface PDFBatchObjectDao {

	/**
	 * 指定したオブジェクトIDのDBレコードの行に排他ロックをかけます。
	 *
	 * @param id 排他ロック対象のオブジェクトID
	 *
	 * @return 処理結果
	 * 	1:指定オブジェクトIDのDBレコードが存在し、排他ロックを掛けた。
	 *  0:指定オブジェクトIDのDBレコードが存在せず、排他ロックを掛けない。
	 * -1:指定オブジェクトIDのDBレコードが既に排他ロックが掛かっている。
	 * @throws Exception 例外<br>
	 *
	 */
	public int lockObjectById(long id) throws Exception;

}
