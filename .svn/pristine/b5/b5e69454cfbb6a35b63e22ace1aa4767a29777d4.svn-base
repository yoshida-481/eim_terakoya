package common.util;

import eim.bo.EIMException;
import eim.util.EIMConfig;
/*
 * オプションを保持するクラス
 */

public class OptionConfData{


	private static OptionConfData instance = null;

	public boolean PDFSetPublicFileFlg = false;		//公開ファイルセキュリティ設定
	public boolean digitalSignFlg      = false;		//署名暗号化
	public boolean searchPageFlg       = false;		//ページ分割検索
	public boolean SignAndEncrFlg	   = false;		//署名・暗号化
	public boolean asperaTransferFlg   = false;		//アスペラ転送
	public boolean ocrFlg = false;					//OCR
	public boolean hgscanConvPdf       = false;		//HGScanコンバーター使用設定
	public boolean insertURLFlag = false;			//URL挿入
	public boolean convertPaperToPDFFlg = false;		 //紙文書電子化
	public boolean enableApproverCheckin = false;	//承認中文書チェックイン可否フラグ

	public void getJudgeData() throws EIMException{

		String optionStr = EIMConfig.getValue("OPTION_ARRAY");
		String[] optList = optionStr.split(",");
		for(int i= 0;i < optList.length; i++){
			if(optList[i].equals("pdf_output_conf")){
				PDFSetPublicFileFlg = true;
			}
			if(optList[i].equals("digital_signature")){
				digitalSignFlg = true;
			}
			if(optList[i].equals("search_page")){
				searchPageFlg = true;
			}
			if(optList[i].equals("signature_and_encryption")){
				SignAndEncrFlg = true;
			}
			if(optList[i].equals("aspera_transfer")){
				asperaTransferFlg = true;
			}
			if(optList[i].equals("ocr")){
				ocrFlg = true;
			}
			if(optList[i].equals("hgpscan_conv_pdf")){
				hgscanConvPdf = true;
			}
			if(optList[i].equals("insert_url")){
				insertURLFlag = true;
			}
			if(optList[i].equals("convert_paper_to_pdf")){
				convertPaperToPDFFlg = true;
			}
			if(optList[i].equals("enable_approver_checkin")){
				enableApproverCheckin = true;
			}
		}

	}
	public static OptionConfData getInstance() throws EIMException{
		if(instance == null){
			instance = new OptionConfData();
			instance.getJudgeData();
		}
		return instance;
	}


}
