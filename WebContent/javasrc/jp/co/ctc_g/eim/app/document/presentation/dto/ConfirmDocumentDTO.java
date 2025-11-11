package jp.co.ctc_g.eim.app.document.presentation.dto;

public class ConfirmDocumentDTO {

		/* オブジェクト名 */
		private String objName = "";

		/* ドキュメント登録パス */
		private String path = "";

		/* オブジェクトタイプ */
		private String objType = "";

		/* アップロードステータス */
		private String uploadStatus = "";

		/* チェックインフラグ */
		private boolean checkinFlag ;

		/* ドキュメント登録不可理由 */
		private String reason = "";

	    // デフォルトコンストラクタ
	    public ConfirmDocumentDTO() {}

       // コンストラクタ
        public ConfirmDocumentDTO(String objName, String path, String objType,
                     String uploadStatus, boolean checkinFlag, String reason) {
        this.objName = objName;
        this.path = path;
        this.objType = objType;
        this.uploadStatus = uploadStatus;
        this.checkinFlag = checkinFlag;
        this.reason = reason;
    }




		public String getObjName() {
			return objName;
		}

		public void setObjName(String objName) {
			this.objName = objName;
		}

		public String getPath() {
			return path;
		}

		public void setPath(String path) {
			this.path = path;
		}

		public String getObjType() {
			return objType;
		}

		public void setObjType(String objType) {
			this.objType = objType;
		}

		public String getUploadStatus() {
			return uploadStatus;
		}

		public void setUploadStatus(String uploadStatus) {
			this.uploadStatus = uploadStatus;
		}

		public boolean getCheckinFlag() {
			return checkinFlag;
		}

		public void setCheckinFlag(boolean checkinFlag) {
			this.checkinFlag = checkinFlag;
		}

		public String getReason() {
			return reason;
		}

		public void setReason(String reason) {
			this.reason = reason;
		}






}
