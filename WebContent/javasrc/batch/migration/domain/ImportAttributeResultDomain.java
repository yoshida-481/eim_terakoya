package batch.migration.domain;

import java.util.ArrayList;
import java.util.List;

/**
 * CSV属性登録処理結果オブジェクト
 * データ1行に対しての属性取込結果を保持する
 *
 * @author z2h2238
 *
 */
public class ImportAttributeResultDomain {

	/**
	 * エラーメッセージリスト
	 */
	private List<String> tempErrorMessage = new ArrayList<String>();

	/**
	 * データの取込みフラグ
	 * true: 取込 false: 処理をスキップ
	 */
	private boolean isUpdated = false;

	/**
	 * エラーメッセージリストの取得
	 * @return
	 */
	public List<String> getTempErrorMessage() {
		return tempErrorMessage;
	}

	/**
	 * エラーメッセージリストの設定
	 * @param tempErrorMessage
	 */
	public void setTempErrorMessage(List<String> tempErrorMessage) {
		this.tempErrorMessage = tempErrorMessage;
	}

	/**
	 * データの取込みフラグの取得
	 * true: 取込 false: 処理をスキップ
	 * @return
	 */
	public boolean isUpdated() {
		return isUpdated;
	}

	/**
	 * データの取込みフラグの取得の設定
	 * @param isUpdated
	 */
	public void setUpdated(boolean isUpdated) {
		this.isUpdated = isUpdated;
	}

}
