package batch.util;

import eim.bo.EIMAttribute;
import eim.bo.EIMAttributeType;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectAttributeUtils;


/**
 *
 * PDFバッチ処理用属性タイプ共通処理クラス
 *
 */

public class PDFAttributeUtil {

	/**
	 *PDFセキュリティ設定オブジェクト属性値をPDF署名オブジェクトへコピー
	 *
	 * @param sess EIMSessionインスタンス
	 * @param pdfsecObj PDFセキュリティ設定オブジェクト
	 * @param pdfsigObj PDF署名オブジェクト
	 * 
	 * @throws Exception
	 */
	public static void copyPDFSecToSigObj(EIMSession sess, EIMObject pdfsecObj, EIMObject pdfsigObj)
	throws Exception
	{
		//「PDFセキュリティ設定」オブジェクトの属性値を「PDF署名」オブジェクトの属性値にコピー
		EIMAttribute secOn = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"));
		EIMAttribute secPassOn = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"));
		EIMAttribute secPass = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
		EIMAttribute refPassOn = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"));
		EIMAttribute refPass = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));
		EIMAttribute secPrint = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"));
		EIMAttribute secEdit = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"));
		EIMAttribute secRem = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"));
		EIMAttribute secRepr = pdfsecObj.getAttribute(EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"));

		EIMAttributeType secOnType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_ONOFF"));
		EIMAttributeType secPassOnType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS_ONOFF"));
		EIMAttributeType secPassType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECPASS"));
		EIMAttributeType refPassOnType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS_ONOFF"));
		EIMAttributeType refPassType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_REFPASS"));
		EIMAttributeType secPrintType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBPRINT"));
		EIMAttributeType secEditType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBEDIT"));
		EIMAttributeType secRemType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBADDREM"));
		EIMAttributeType secReprType = AttributeUtils.getAttributeTypeByName
									(sess, EIMConfig.get("ATTR_NAME_WFPUB_PDF_SECURITY_FORBREPR"));

		if(secOn != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, secOnType, secOn.getInt());
		}
		if(secPassOn != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, secPassOnType, secPassOn.getInt());
		}
		if(secPass != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, secPassType, secPass.getString());
		} else {
			ObjectAttributeUtils.deleteAttribute(sess, pdfsigObj, secPassType);
		}
		if(refPassOn != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, refPassOnType, refPassOn.getInt());
		}
		if(refPass != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, refPassType, refPass.getString());
		} else {
			ObjectAttributeUtils.deleteAttribute(sess, pdfsigObj, refPassType);
		}
		if(secPrint != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, secPrintType, secPrint.getInt());
		}
		if(secEdit != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, secEditType, secEdit.getInt());
		}
		if(secRem != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, secRemType, secRem.getInt());
		}
		if(secRepr != null)
		{
			ObjectAttributeUtils.setAttribute(sess, pdfsigObj, secReprType, secRepr.getInt());
		}
	}

}
