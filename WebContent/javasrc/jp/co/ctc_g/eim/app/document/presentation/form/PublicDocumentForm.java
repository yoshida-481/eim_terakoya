package jp.co.ctc_g.eim.app.document.presentation.form;

/**
* PublicDocumentController#createAsync()が受け取る引数のフォームです
*@since Ver1.0
*/
public class PublicDocumentForm {

	/** オブジェクトID */
	private long[] ids;

	/**
	 * コンストラクタ
	 *
	 * @since Ver1.0
	 */
	public PublicDocumentForm(){

	}


	/**
	 * オブジェクトIDを取得します。
	 *
	 * @return オブジェクトID
	 * @since Ver1.0
	 */
	public long[] getIds() {
		return ids;
	}

	/**
	 * オブジェクトIDを設定します。
	 *
	 * @param id オブジェクトID
	 * @since Ver1.0
	 */
	public void setIds(long[] ids) {
		this.ids = ids;
	}





}
