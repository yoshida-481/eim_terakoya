package common.tools;


import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import common.tools.internal.AppObjectTypeUtils;
import common.tools.internal.AppPathAttibuteUtils;
import common.tools.internal.PathDomain;
import common.tools.internal.PathTypeEnum;
import common.util.AppConstant;
import common.util.AppMessageUtils;
import common.util.CsvOutputUtil;
import eim.bo.EIMAttribute;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.net.EIMSession;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eim.util.StringUtils;
import eim.util.TypeConvertUtils;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class PathInfoListUp extends Thread{

	/** エスケープ済ダブルクォーテーション */
	private static String ESCAPE_DQ;
	/** CSV列デリミタ(タブ) */
	private static String CSV_COLDELIMITER = "\t";
	/** 改行文字 */
	private static String CSV_NEWLINE = "\r\n";
	
	/**  オブジェクトネーム：ワークスベース */
	private static String OBJECT_TYPE_NAME_WORKSPACE;
	/**  オブジェクトネーム：ゴミ箱 */
	private static String OBJECT_TYPE_NAME_RECYCLE;
	/**  オブジェクトネーム：フォルダ */
	private static String OBJECT_TYPE_NAME_FOLDER;
	/**  オブジェクトネーム：ドキュメント */
	private static String OBJECT_TYPE_NAME_DOCUMENT;
	/**  オブジェクトネーム：タグ */
	private static String OBJECT_TYPE_NAME_TAG;
	/**  属性タイプ名称：パス */
	private static String ATTR_NAME_DOCUMENT_PASS;
	/**  属性タイプ名称：リンス先 */
	private static String ATTR_NAME_DOCUMENT_TARGET_TO_LINK;
	
	private static SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
	
	StringBuffer sbOk = null;
	StringBuffer sbNg = null;
	
	List<Integer> ids;
	String target;

	
	/**
	 * ファイル出力フラグ
	 * OK : OKのみ出力
	 * NG : NGのみ出力
	 * ALL : 全て出力する
	 */
	public static enum PrintModeEnum {
		OK("OK",1), NG("NG", 2), ALL("ALL", 3);
		private String name;
		private int value;
		
		private PrintModeEnum(String name, int value){
			this.name = name;
			this.value = value;
		}
		public int getValue() {
			return value;
		}
		public String getName() {
			return name;
		}
	}
	
	
	
	private static PrintModeEnum printMode = null;
	
	/**
	 * 正しいデータのtsvファイル出力バッファ
	 */
	private static BufferedWriter okBw = null;
	
	
	/**
	 * 不正なデータのtsvファイル出力バッファ
	 */
	private static BufferedWriter ngBw = null;
	
	private static Log log = null;
	
	
	public PathInfoListUp(List<Integer> ids, String target){
		this.ids = ids;
		this.target = target;
	}
	
	
	static{
		try {
			log = LogFactory.getLog(PathInfoListUp.class);
			
			ESCAPE_DQ = AppConstant.CSV_ESCDQUOTATION + "\"";
			OBJECT_TYPE_NAME_WORKSPACE = EIMConfig.getValue("OBJECT_TYPE_NAME_WORKSPACE");
			OBJECT_TYPE_NAME_RECYCLE = EIMConfig.getValue("OBJECT_TYPE_NAME_RECYCLE");
			OBJECT_TYPE_NAME_DOCUMENT = EIMConfig.getValue("OBJECT_TYPE_NAME_DOCUMENT");
			OBJECT_TYPE_NAME_TAG = EIMConfig.getValue("OBJECT_TYPE_NAME_TAG");
			
			ATTR_NAME_DOCUMENT_PASS = EIMConfig.getValue("ATTR_NAME_DOCUMENT_PASS");
			ATTR_NAME_DOCUMENT_TARGET_TO_LINK = EIMConfig.getValue("ATTR_NAME_DOCUMENT_TARGET_TO_LINK");
			
			
		} catch (Exception e) {
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		}
	}
	
	
	
	/**
	 * 引数で指定されたフォルダ(arg[0])に実行した日付のフォーマットYYYYMMDDHHmmss_ORACLEHOST=ホスト名_ORACLESID=SID_ORACLEUSER=スキーマ
	 * @param args 
	 * args[0] ファイルが作成されるフォルダ<br>
	 * args[1] ファイル出力モード <br>
	 *    ok : 正しいデータ<br>
	 *    ng : 不正のデータ<br>
	 *    all : 全てのデータ<br>
	 * args[2] 処理対象<br>
	 *    workspace : <br>
	 *    document  : <br>
	 *    recycle   : <br>
	 *    tag       : <br>
	 */
	public static void main(String[] args) {
		
		if(args.length != 2){
			System.out.println("args error");
			return;
		}
		
		File listFilePath = new File(args[0]);
		if(!listFilePath.canWrite() || !listFilePath.isDirectory()){
			System.out.println("args folder can not write");
			return;
		}
		
		if(args[1].toUpperCase().equals("OK")){
			printMode = PrintModeEnum.OK;
		}else if(args[1].toUpperCase().equals("NG")){
			printMode = PrintModeEnum.NG;
		}else if(args[1].toUpperCase().equals("ALL")){
			printMode = PrintModeEnum.ALL;
		}else{
			System.out.println("args not write");
			return;
		}
		
		
		
		//時刻まで指定しているので、以前作成したものと一致する事はないはずだが、必ず新規にファイルを作成する
		MessageFormat mf = new MessageFormat("{0,date,yyyyMMddHHmmss}");
		Object[] objs = {Calendar.getInstance().getTime()};
		
		
		
		log.info(" start PathInfoListUp");

		EIMSession sess = null;
		try {
			sess = new EIMSession();
			sess.setConsole();
			EIMThreadContext.putEIMSession(sess);

			// 出力文字コード取得
			String charset = CsvOutputUtil.getCharCodeSetting(sess);
			
			StringBuffer head = new StringBuffer();
			head.append(replaceCsvString("ID",false));
			head.append(replaceCsvString("履歴",false));
			head.append(replaceCsvString("名称",false));
			head.append(replaceCsvString("リレーションタイプ",false));
			head.append(replaceCsvString("リレーションパス",false));
			head.append(replaceCsvString("パス属性",false));
			head.append(replaceCsvString("リンクID",true));
			
			
			if(printMode != PrintModeEnum.NG){ 
				String okFileName = listFilePath.getPath() + "/" + mf.format(objs) +"_ORACLEHOST=" + EIMConfig.getValue("DB_HOST") + "_ORACLESID=" + EIMConfig.getValue("DB_ID") + "_ORACLEUSER=" + EIMConfig.getValue("DB_USER") + "_OK.tsv";
				okBw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(okFileName), charset));

				okBw.write(new String(head)); 
			}
			
			if(printMode != PrintModeEnum.OK){ 
				String ngFileName = listFilePath.getPath() + "/" + mf.format(objs) +"_ORACLEHOST=" + EIMConfig.getValue("DB_HOST") + "_ORACLESID=" + EIMConfig.getValue("DB_ID") + "_ORACLEUSER=" + EIMConfig.getValue("DB_USER") + "_NG.tsv";
			    ngBw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(ngFileName), charset));

			    ngBw.write(new String(head)); 
			}
			
			
			List<Integer> workSpaceIds = AppObjectTypeUtils.getListByInheritanceTypeName(OBJECT_TYPE_NAME_WORKSPACE);
			List<Integer> garbageIds = AppObjectTypeUtils.getListByInheritanceTypeName(OBJECT_TYPE_NAME_RECYCLE);
			List<Integer> documentIds = AppObjectTypeUtils.getListByInheritanceTypeName(OBJECT_TYPE_NAME_DOCUMENT);
			List<Integer> tagIds = AppObjectTypeUtils.getListByInheritanceTypeName(OBJECT_TYPE_NAME_TAG);
			
			System.out.println(OBJECT_TYPE_NAME_WORKSPACE + "(フォルダ) : " + String.valueOf(workSpaceIds.size()));
			System.out.println(OBJECT_TYPE_NAME_RECYCLE + " : " + String.valueOf(garbageIds.size()));
			System.out.println(OBJECT_TYPE_NAME_DOCUMENT + " : " + String.valueOf(documentIds.size()));
			System.out.println(OBJECT_TYPE_NAME_TAG + " : " + String.valueOf(tagIds.size()));
			
			
			
			
			PathInfoListUp ws = new PathInfoListUp(workSpaceIds, OBJECT_TYPE_NAME_WORKSPACE);
			PathInfoListUp recy = new PathInfoListUp(garbageIds, OBJECT_TYPE_NAME_RECYCLE);
			PathInfoListUp tag = new PathInfoListUp(tagIds, OBJECT_TYPE_NAME_TAG);
			
			
			int exeListFrom = 0;
			// StringBufferの内容をファイルに書き込む時間の為、3ではなく3.3に分ける（150000件で最適）
			int exeListTo = (int) (documentIds.size() / 3.3);
			PathInfoListUp doc1 = new PathInfoListUp(documentIds.subList(exeListFrom, exeListTo), OBJECT_TYPE_NAME_DOCUMENT + "_1");
			
			exeListFrom = exeListTo;
			exeListTo = (int)(exeListTo * 2.1);
			PathInfoListUp doc2 = new PathInfoListUp(documentIds.subList(exeListFrom, exeListTo), OBJECT_TYPE_NAME_DOCUMENT+ "_2");
			
			exeListFrom = exeListTo;
			exeListTo = documentIds.size();
			PathInfoListUp doc3 = new PathInfoListUp(documentIds.subList(exeListFrom, exeListTo), OBJECT_TYPE_NAME_DOCUMENT+ "_3");
			
			
			if (okBw != null){
				ws.sbOk = new StringBuffer(10000);
				recy.sbOk = new StringBuffer(5000);
				tag.sbOk = new StringBuffer(5000);
				doc1.sbOk = new StringBuffer(10000);
				doc2.sbOk = new StringBuffer(10000);
				doc3.sbOk = new StringBuffer(10000);
			}
			
			if (ngBw != null){
				ws.sbNg = new StringBuffer(10000);
				recy.sbNg = new StringBuffer(1000);
				tag.sbNg = new StringBuffer(1000);
				doc1.sbNg = new StringBuffer(10000);
				doc2.sbNg = new StringBuffer(10000);
				doc3.sbNg = new StringBuffer(10000);
			}
			
			
			ws.start();
			doc1.start();
			doc2.start();
			doc3.start();
			
			
			ws.join();
			recy.start();
			if (okBw != null){
				okBw.write("■" + OBJECT_TYPE_NAME_WORKSPACE + "(フォルダ)"); 
	            okBw.newLine();
				okBw.write(ws.sbOk.toString());
			}
			
			if (ngBw != null){
				ngBw.write("■" + OBJECT_TYPE_NAME_WORKSPACE + "(フォルダ)"); 
				ngBw.newLine();
				ngBw.write(ws.sbNg.toString());
			}
			
			
			recy.join();
			tag.start();
			if (okBw != null){
				okBw.write("■" + OBJECT_TYPE_NAME_RECYCLE); 
	            okBw.newLine();
				okBw.write(recy.sbOk.toString());
			}
			if (ngBw != null){
				ngBw.write("■" + OBJECT_TYPE_NAME_RECYCLE); 
				ngBw.newLine();
				ngBw.write(recy.sbNg.toString());
			}
			
			
			
			
			doc1.join();
			if (okBw != null){
				okBw.write("■" + OBJECT_TYPE_NAME_DOCUMENT); 
				okBw.newLine();
				okBw.write(doc1.sbOk.toString());
			}
			
			if (ngBw != null){
				ngBw.write("■" + OBJECT_TYPE_NAME_DOCUMENT); 
				ngBw.newLine();
				ngBw.write(doc1.sbNg.toString());
			}
			
			
			doc2.join();
			if (okBw != null){
				okBw.write(doc2.sbOk.toString());
			}
			
			if (ngBw != null){
				ngBw.write(doc2.sbNg.toString());
			}
			
			
			doc3.join();
			if (okBw != null){
				okBw.write(doc3.sbOk.toString());
			}
			
			if (ngBw != null){
				ngBw.write(doc3.sbNg.toString());
			}
			
			
			tag.join();
			if (okBw != null){
				okBw.write("■" + OBJECT_TYPE_NAME_TAG); 
	            okBw.newLine();
				okBw.write(tag.sbOk.toString());
			}
			
			if (ngBw != null){
				ngBw.write("■" + OBJECT_TYPE_NAME_TAG); 
				ngBw.newLine();
				ngBw.write(tag.sbNg.toString());
			}
			

			
		}
		catch(Exception eime){
			log.warn(AppMessageUtils.makeLogMessage(eime.getMessage()), eime);

		}
		finally{
			
			try {

				if(okBw != null) {
					okBw.close();
				}
				if(ngBw != null){
					ngBw.close();
				}
			} catch (IOException e1) {
				log.error(AppMessageUtils.makeLogMessage(e1.getMessage()), e1);
			}
			try{
				if(sess != null)
					sess.close();
			}
			catch(Exception e){
				log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			}
			log.info(" end PathInfoListUp");
		}
	}
	
	
	
	
	/**
	 * 文字列をエスケープする。<BR>
	 * エスケープ方法は、Excel2003に従う。
	 * @param str String
	 * 
	 * @return String エスケープしてダブルクォーテーションで囲んだ文字列
	 */
	private static String replaceCsvString(String str, boolean endWordFlg){	

		str = StringUtils.convertReturnCede(str);
		str = str.replaceAll("\"", ESCAPE_DQ);

		
		if (endWordFlg) {
			return "\"" + str + "\"" + CSV_NEWLINE ;
		}
		return "\"" + str + "\"" + CSV_COLDELIMITER;
	}
	
	
	
	private void writePathInfo(List<Integer> ids, String target){
		
		EIMSession sess = null;
		try {
			sess = new EIMSession();
			sess.setConsole();
			
		
		
		EIMThreadContext.putEIMSession(sess);

		for(int idIndex = 0 ; ids.size() > idIndex ; idIndex++){
			
			
			
			int objId = ids.get(idIndex);
			
			
			// 処理進行状況を画面に出力
			if(idIndex % 5000 == 0 ){
				System.out.println(target + " : "  + idIndex + " / " + ids.size() + "\t\t" + sdf.format(new Date()));
			}
			
			EIMObject object = ObjectUtils.getObjectById(sess, objId);
			
			
			List<PathDomain> pathInfoList = AppPathAttibuteUtils.getPathByRelation(object, true);
			if(31665 == object.getId()){
				int test = 0;
			}
			for(int pathInfoListIndex = 0;  pathInfoListIndex < pathInfoList.size(); pathInfoListIndex++){
				
				StringBuffer sb = new StringBuffer(150);
				
				// 正否フラグ
				boolean ngFlg = false;
				
				PathDomain pathInfo = pathInfoList.get(pathInfoListIndex);
				
				// ID
				sb.append(replaceCsvString(String.valueOf(objId), false));
				
				//履歴
				sb.append(replaceCsvString(String.valueOf(object.getRev()), false));
				
				// 名称
				sb.append(replaceCsvString(object.getName(), false));
				
				// リレーションタイプ
				if(pathInfo.getType() == null){
					ngFlg = true;
					sb.append(replaceCsvString(null, false));
				}else{
					sb.append(replaceCsvString(pathInfo.getType().getSymbol(), false));
				}
				
				ngFlg = (pathInfo.getType() == PathTypeEnum.IRREGULAR_NORELATION || pathInfo.getType() == PathTypeEnum.IRREGULAR_LATEST_NOTFOUND);
				// Rootの場合
				if(isRootTypeObject(object) || pathInfo.getType() == PathTypeEnum.MYDOCUMENT || pathInfo.getType() == PathTypeEnum.IRREGULAR_NORELATION || pathInfo.getType() == PathTypeEnum.IRREGULAR_LATEST_NOTFOUND){
					sb.append(replaceCsvString(null, false));
					sb.append(replaceCsvString(null, false));
					sb.append(replaceCsvString(null, true));
					setStringBuffer(sb, ngFlg);
					continue;
				}
				
				// リレーションパス
				String relPath = pathInfo.getRealPath();
				if(StringUtils.isBlank(relPath)){
					ngFlg = true;
				}
				sb.append(replaceCsvString(relPath, false));
				
				
				// パス属性
				EIMAttribute objectPath = object.getAttribute(ATTR_NAME_DOCUMENT_PASS);
				String [] pathAttr = null;
				
				if (objectPath == null){
					ngFlg = true;
					sb.append(replaceCsvString(null, false));
					
				}else{
					pathAttr = objectPath.getStrings();
					
					
					// フォルダ、ワークスベース、ゴミ箱で複数パス属性がある場合
					if(pathAttr != null && pathAttr.length > 1 
							&&( target.equals(OBJECT_TYPE_NAME_FOLDER) 
							|| target.equals(OBJECT_TYPE_NAME_WORKSPACE)
							|| target.equals(OBJECT_TYPE_NAME_RECYCLE))){
						
						ngFlg = true;
						sb.append(replaceCsvString(pathAttr[0], false));
						sb.append(replaceCsvString(null, true));
						
						for(int i = 1; pathAttr.length > i ; i++){
							sb.append(replaceCsvString(null, false));
							sb.append(replaceCsvString(null, false));
							sb.append(replaceCsvString(null, false));
							sb.append(replaceCsvString(null, false));
							sb.append(replaceCsvString(null, false));
							sb.append(replaceCsvString(pathAttr[i], false));
							sb.append(replaceCsvString(null, true));
							
						}
						setStringBuffer(sb, ngFlg);
						continue;
					}
					
					if(pathAttr != null && pathAttr.length > pathInfoListIndex){
						sb.append(replaceCsvString(pathAttr[pathInfoListIndex], false));
						
						// リレーションパス≠パス属性
						if(!pathAttr[pathInfoListIndex].equals(relPath)){
							ngFlg = true;
						}
						
						
						
					}else{
						// パス属性がNULL又はパス属性件数＜パス情報件数
						ngFlg = true;
						sb.append(replaceCsvString(null, false));
					}
					
				}
				

				// リンクID
				if(pathInfoListIndex > 0){
					long [] linkId = null; 
					EIMAttribute objectLink = object.getAttribute(ATTR_NAME_DOCUMENT_TARGET_TO_LINK);
					if (objectLink == null){
						ngFlg = true;
						sb.append(replaceCsvString(null, true));

					}else{
						linkId = TypeConvertUtils.convertToLongArray(objectLink.getInts());
						if(linkId.length >= pathInfoListIndex){
							sb.append(replaceCsvString(String.valueOf(linkId [pathInfoListIndex-1]), true));
						}else{
							ngFlg = true;
							sb.append(replaceCsvString(null, true));
						}
					}
				}else{
					
					sb.append(replaceCsvString(null, true));
				}
				
				
				setStringBuffer(sb, ngFlg);
				
				
				if(pathAttr != null && pathInfoList.size() == pathInfoListIndex +1 && pathAttr.length > pathInfoList.size()){
					// パス情報の最後ライン且つ、出力されないパス属性がある場合、残りのパス属性を出力する。
					
					// StringBufferをクリアする。
					sb.delete(0, sb.length());
					
					ngFlg = true;
					for(int i = pathInfoList.size(); pathAttr.length > i ; i++){
						
						sb.append(replaceCsvString(String.valueOf(objId), false));
						sb.append(replaceCsvString(null, false));
						sb.append(replaceCsvString(null, false));
						sb.append(replaceCsvString(null, false));
						sb.append(replaceCsvString(null, false));
						sb.append(replaceCsvString(pathAttr[i], false));
						sb.append(replaceCsvString(null, true));
						
					}
					setStringBuffer(sb, ngFlg);
					
					continue;
				}

			}

		}
		
		} catch (Exception e) {
			
			log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
		}finally{
			
			
			try{
				if(sess != null)
					sess.close();
			}
			catch(Exception e){
				log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			}
		}
		
	}
	
	
	private void setStringBuffer(StringBuffer strB, boolean ngFlg ) { 
		if(!ngFlg && printMode != PrintModeEnum.NG){
			sbOk.append(strB);
		}
		
		if(ngFlg && printMode != PrintModeEnum.OK){
			sbNg.append(strB);

		}
	}
	
	public void run() {
        try {
        	writePathInfo(this.ids, this.target);

        } catch (Exception e) {
        	log.error(AppMessageUtils.makeLogMessage(e.getMessage()), e);
			
		}
	}
	
	
	
	private static boolean isRootTypeObject(EIMObject object) throws EIMException{
		return object.getType().getDefName().equals(OBJECT_TYPE_NAME_WORKSPACE) || 
			object.getType().getDefName().equals(OBJECT_TYPE_NAME_RECYCLE);
	}
	
}


