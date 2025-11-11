package jp.co.ctc_g.eim.app.document.presentation.dto;

/**
 * Box→EIM 登録ドキュメント情報DTO
 */
public class BoxDocumentCreateDTO {

		/* オブジェクト名 */
		private String objName = "";

		/* オブジェクト名 */
		private Long objId = 0L;

		/* エラーメッセージ */
		private String message = "";

	    // デフォルトコンストラクタ
	    public BoxDocumentCreateDTO() {}

		public String getObjName() {
			return objName;
		}

		public void setObjName(String objName) {
			this.objName = objName;
		}

		public Long getObjId() {
			return objId;
		}

		public void setObjId(Long objId) {
			this.objId = objId;
		}

		public String getMessage() {
			return message;
		}

		public void setMessage(String message) {
			this.message = message;
		}

}
