package batch.migration.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import batch.migration.domain.ImportAttributeResultDomain;
import common.bo.AttributeValueMaster;
import common.util.AppConstant;
import common.util.AppUpdateNoticeUtils;
import common.util.AttributeMasterUtil;
import eim.bo.EIMAccessRole;
import eim.bo.EIMException;
import eim.bo.EIMObject;
import eim.bo.EIMResource;
import eim.net.EIMSession;
import eim.util.AccessUtils;
import eim.util.EIMConfig;
import eim.util.EIMConstant;
import eim.util.OperationHistoryUtils;
import eim.util.StringUtils;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AccessRoleTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.AttributeTypeDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectDomain;
import jp.co.ctc_g.eim.framework2.business.domain.entity.ObjectTypeDomain;
import jp.co.ctc_g.eim.framework2.business.service.ObjectService;
import jp.co.ctc_g.eim.framework2.business.service.ObjectTypeService;
import jp.co.ctc_g.eim.framework2.common.util.ConfigUtils;
import jp.co.ctc_g.eim.framework2.common.util.ResourceUtils;


/**
 * CSV属性一括登録バッチ属性更新処理実行
 *
 */
public class UpdateAttributeUtils{

	/**
	 * オブジェクトタイプドメインのキャッシュ
	 */
	private static Map<String, ObjectTypeDomain> objectTypeCache = new HashMap<String, ObjectTypeDomain>();

	/**
	 * 属性タイプ値マスターのキャッシュ
	 */
	private static Map<String, AttributeValueMaster> attrListCheckCache = new HashMap<String, AttributeValueMaster>();

	/**
	 * ルートタイプチェックのキャッシュ
	 */
	private static Map<String, String> typeCheckCache = new HashMap<String, String>();

	// 数値型属性の最大入力桁数を設定する
	private static final int INTEGER_ATTR_MAX_CHARS = 9;

	// 実数型属性の最大入力桁数を設定する
	private static final int REAL_NUMBER_ATTR_MAX_CHARS = 17;

	// 文字列型属性の最大入力桁数を設定する
	private static final int STRING_ATTR_MAX_CHARS = 127;

	// 小数入力チェックパターン
	private static Pattern DOUBLE_PATERN = Pattern.compile("-?([1-9]\\d*|0)(\\.\\d+)?$");

	/**
	 * ドキュメントの属性登録実行
	 *
	 * @param targetHeaderList ヘッダリスト（インポート対象外のヘッダを""に置き換えたもの）
	 * @param csvRow インポート対象行
	 * @param objectTypeService オブジェクトタイプサービス
	 * @param objectService オブジェクトサービス
	 * @param sess セッション
	 * @return 処理結果オブジェクト
	 * @throws Exception 例外
	 */
	public static ImportAttributeResultDomain executeImport(
			List<String> targetHeaderList, String[] csvRow,
			ObjectService objectService, ObjectTypeService objectTypeService,
			EIMSession sess) throws Exception {
		// 返却用のエラーメッセージリスト
		List<String> errorMessagesList = new ArrayList<String>();
		// 各項目の値
		String csvRowCols = null;
		// カラム単位で更新する/しないを判定するフラグ。trueになった場合、その行は属性更新しない
		boolean columnNotUpdateFlag = false;

		// すべてのカラムが取込対象外か判定するフラグ falseの場合、その行は更新しない(処理が無駄なのでスキップする、履歴も出さない)
		boolean allBlankChk = false;

		csvRowCols = csvRow[targetHeaderList.size() - 1];// 処理対象行の文書IDを取得

		long documentId = 0;

		// 文書IDがNull、もしくは""の場合、インポート処理を終了
		if (csvRowCols == null || StringUtils.isBlank(csvRowCols)) {
			errorMessagesList.add("文書IDが存在しません。");

			return returnErrorResult(errorMessagesList, false);
		}

		// 文書IDが数値に変換できない場合、本行の処理をスキップ
		try {
			documentId = Long.parseLong(csvRowCols);
		} catch (NumberFormatException nfex) {
			Object[] messageParams = { csvRowCols };
			errorMessagesList.add(EIMResource
					.getMessage("EIM.ERROR.INCSV.DOCUMENT.ID.ILLEGAL", messageParams));
			return returnErrorResult(errorMessagesList, false);
		}

		ObjectDomain documentDomain = objectService.getById(documentId);
		List<String> chekcResultList = checkDocumentBeforeImport(
				documentDomain, documentId, objectService, objectTypeService);

		// チェックの結果、エラーが返却された場合、エラーメッセージを格納し本行の処理をスキップ
		if (chekcResultList.size() > 0) {
			for (String s : chekcResultList) {
				errorMessagesList.add(s);
			}
			return returnErrorResult(errorMessagesList, false);
		}

		long docObjectTypeId = documentDomain.getType().getId();
		String cacheKey = String.valueOf(docObjectTypeId);

		// ドキュメントタイプの取得とキャッシュ処理
		ObjectTypeDomain documentObjectTypeDomain = null;
		if (objectTypeCache.containsKey(cacheKey)) {

			documentObjectTypeDomain = objectTypeCache.get(cacheKey);
		} else {
			documentObjectTypeDomain = objectTypeService.getById(docObjectTypeId);
			objectTypeCache.put(cacheKey, documentObjectTypeDomain);
		}

		// 属性タイプリストを KEY=定義名 VALUE=ドメインのマップに変換
		Map<String, AttributeTypeDomain> documentAttrTypeMap = convertAtriibuteTypeList(documentObjectTypeDomain);

		// 更新用にオブジェクトの全属性を取り直す
		List<AttributeDomain> currentAttributeList = documentDomain
				.getAttributeList();
		// 処理のためマップに変換 KEY=属性タイプの定義名称 VALUE=属性ドメイン
		Map<String, AttributeDomain> importAttributeMap = convertAtriibuteList(currentAttributeList);

		// 値部の処理
		TOP_LOOP:
		for (int i = 0; i < targetHeaderList.size(); i++) {
			AttributeDomain importAttribute = new AttributeDomain();

			// ヘッダ名が""に設定されているものは、インポート対象外としてスキップ
			if ("".equals(targetHeaderList.get(i))) {
				// 基本属性のスキップの場合、エラーメッセージは生成せず、スキップ
				continue;
			}


			String targetDefName = targetHeaderList.get(i); // インポートデータヘッダ定義名称

			// 該当する項目がない場合はスキップ
			if (documentAttrTypeMap.containsKey(targetDefName)) {

				AttributeTypeDomain at = documentAttrTypeMap.get(targetDefName);

				csvRowCols = csvRow[i];

				// メッセージの置換文字列
				Object[] messageParams = { String.valueOf(documentId), at.getDefinitionName() };

				// 対象の属性が「上位からの引継ぎ属性」の場合、登録対象としない
				if (getInheritedAttList(documentDomain, at)) {
					// 「上位からの引継ぎ属性」の場合、エラーメッセージはセットせず、スキップ
					continue;
				} else {
					allBlankChk = true; //最低1件は更新対象データなのでフラグtrueにする
				}

				// /////////////////////////////////////////////////
				// ブランクを入力した場合は属性を削除で更新
				// /////////////////////////////////////////////////
				if (StringUtils.isBlank(csvRowCols)) {

					importAttributeMap.remove(targetDefName);
					continue;
				}

				importAttribute.setAttributeType(at);

				// 複数値属性か否かによって、処理を分ける
				if (at.isMultiple()) {
					// パイプ（区切り文字）
					String spliter = "\\|";

					// 複数値の場合（パイプは、区切り文字として処理する）
					AttributeValueMaster attValueMaster = null;
					String impAttDefName = importAttribute.getAttributeType().getDefinitionName();

					// ここで定義するのはおかしいので削除
					// List<String> splitedList = new ArrayList<String>();

					switch (importAttribute.getAttributeType().getValueType()) {
					// 数値型の場合
					case LONG:
						List<Long> longList = new ArrayList<Long>();
						try {
							List<String> splitedLongDataList = splitString(
									csvRowCols, spliter);
							for (String s : splitedLongDataList) {

								longList.add(Long.parseLong(s));

								if (INTEGER_ATTR_MAX_CHARS < s.length()) {
									// 桁数オーバーの場合、文書ID、項目名をエラーメッセージに設定しスキップする
									errorMessagesList.add(EIMResource.getMessage(
															"EIM.ERROR.INCSV.INPUT.INTEGER.VALUE.LENGTH.OVER",
															messageParams));
									columnNotUpdateFlag = true;// フラグが立った行は更新しない
									break TOP_LOOP;
								}
							}
						} catch (NumberFormatException nfex) {
							// parseCheckFlag = false;

							// 数値変換エラーの場合、処理をスキップする。
							errorMessagesList.add(EIMResource.getMessage(
									"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
									messageParams));
							columnNotUpdateFlag = true;// フラグが立った行は更新しない
							break TOP_LOOP;
						}


						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						// 型変換に問題が無い場合、リストを保持しているか判定
						if (attValueMaster == null) {
							// 処理なし
						} else if (longList.size() > 0) {

							long[] ii = attValueMaster.getInts();// リスト値取得

							List<Long> tempLongs = new ArrayList<Long>();
							for (long a : ii) {

								Long in = new Long(a);
								tempLongs.add(in.longValue());
							}

							for (Long l : longList) {
								if (tempLongs.contains(l)) {
									// 処理なし
								} else {
									// リストに存在しない値は登録を許可しない
									errorMessagesList.add(EIMResource.getMessage(
															"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
															messageParams));
									columnNotUpdateFlag = true;// フラグが立った行は更新しない
									break TOP_LOOP;
								}
							}
						}

						// 値を設定
						importAttribute.setLongList(longList);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(impAttDefName, importAttribute);

						break;

					// DATE型の場合
					case DATE:

						List<String> splitedDateDataList = splitString(
								csvRowCols, spliter);

						List<Date> formatDateList = formatDateDataList(splitedDateDataList);

						if (formatDateList == null) {

							// 戻りがnullの場合はフォーマットエラー
							errorMessagesList.add(EIMResource.getMessage(
									"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
									messageParams));
							columnNotUpdateFlag = true;// フラグが立った行は更新しない
							break TOP_LOOP;
						}

						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						// 型変換に問題が無い場合、リストを保持しているか判定
						if (attValueMaster == null) {
							// 処理なし
						} else if (formatDateList.size() > 0) {

							Date[] ii = attValueMaster.getDates();// リスト値取得

							List<Date> tempDates = new ArrayList<Date>();
							for (Date a : ii) {
								tempDates.add(a);
							}

							for (Date d : formatDateList) {
								if (tempDates.contains(d)) {
									// 処理なし
								} else {
									// リストに存在しない値は登録を許可しない
									errorMessagesList.add(EIMResource.getMessage(
															"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
															messageParams));
									columnNotUpdateFlag = true;// フラグが立った行は更新しない
									break TOP_LOOP;
								}
							}
						}

						// 値を設定
						importAttribute.setDateList(formatDateList);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(impAttDefName, importAttribute);

						break;

					// DOUBLE型の場合
					case DOUBLE:
						List<Double> doubleList = new ArrayList<Double>();
						try {
							List<String> splitedDoubleDataList = splitString(csvRowCols, spliter);
							for (String s : splitedDoubleDataList) {
								doubleList.add(Double.parseDouble(s));

								if (REAL_NUMBER_ATTR_MAX_CHARS < s.length()) {
									// 桁数オーバーの場合、文書ID、項目名をエラーメッセージに設定しスキップする
									errorMessagesList.add(EIMResource.getMessage(
															"EIM.ERROR.INCSV.INPUT.REAL.NUMBER.VALUE.LENGTH.OVER",
															messageParams));
									columnNotUpdateFlag = true;// フラグが立った行は更新しない
									break TOP_LOOP;
								}

								if (!checkInputPatern(DOUBLE_PATERN, s)) {

									errorMessagesList.add(EIMResource.getMessage(
															"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
															messageParams));
									columnNotUpdateFlag = true;// フラグが立った行は更新しない
									break TOP_LOOP;
								}

							}

						} catch (NumberFormatException nfex) {

							errorMessagesList.add(EIMResource.getMessage("EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",messageParams));
							columnNotUpdateFlag = true;// フラグが立った行は更新しない
							break TOP_LOOP;
						}

						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						// 型変換に問題が無い場合、リストを保持しているか判定
						if (attValueMaster == null) {
							// 処理なし
						} else if (doubleList.size() > 0) {

							double[] ii = attValueMaster.getDoubles();// リスト値取得

							List<Double> tempDoubles = new ArrayList<Double>();
							for (double a : ii) {
								tempDoubles.add(Double.valueOf(a));
							}

							for (double d : doubleList) {
								if (tempDoubles.contains(d)) {
									// 処理なし
								} else {
									// リストに存在しない値は登録を許可しない
									errorMessagesList.add(EIMResource
													.getMessage("EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",messageParams));
									columnNotUpdateFlag = true;// フラグが立った行は更新しない
									break TOP_LOOP;
								}
							}
						}

						// 値を設定
						importAttribute.setDoubleList(doubleList);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(impAttDefName, importAttribute);

						break;

					// STRING型の場合
					case STRING:
						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						List<String> splitedStrDataList = splitString(csvRowCols, spliter);

						for (String s : splitedStrDataList) {

							if (StringUtils.isBlank(s)) {
								errorMessagesList.add(EIMResource.getMessage("EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}

							if (STRING_ATTR_MAX_CHARS < s.length()) {
								// 桁数オーバーの場合、文書ID、項目名をエラーメッセージに設定しスキップする
								errorMessagesList.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.INPUT.STRING.VALUE.LENGTH.OVER",
														messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}

						}

						if (attValueMaster == null) {
							// 処理なし
						} else if (splitedStrDataList.size() > 0) {

							String[] ii = attValueMaster.getStrings();
							List<String> tempStrings = new ArrayList<String>();
							for (String s : ii) {
								tempStrings.add(s);
							}

							for (String s : splitedStrDataList) {
								if (tempStrings.contains(s)) {
									// 処理なし
								} else {
									// リストに存在しない値は登録を許可しない
									errorMessagesList.add(EIMResource
													.getMessage(
															"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
															messageParams));
									columnNotUpdateFlag = true;// フラグが立った行は更新しない
									break TOP_LOOP;
								}
							}
						}

						// 値を設定（型変換は行わない）
						importAttribute.setStringList(splitedStrDataList);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(impAttDefName, importAttribute);

						break;

					// TEXT型の場合
					case TEXT:
						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						List<String> splitedTextDataList = splitString(csvRowCols, spliter); // 更新データ

						List<String> txtList = new ArrayList<String>(); // 改行エスケープ後の更新データ

						for (String s : splitedTextDataList) {

							if (StringUtils.isBlank(s)) {
								errorMessagesList.add(EIMResource.getMessage("EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}

							// 改行コード\nが存在する場合、\rに置き換える
							s = s.replace("\n", "\r");

							txtList.add(s);
						}

						if (attValueMaster == null) {
							// 処理なし

						} else {

							String[] ii = attValueMaster.getTexts();
							List<String> tempTxts = new ArrayList<String>();
							for (String s : ii) {
								tempTxts.add(s);
							}

							for (String s : txtList) {
								if (tempTxts.contains(s)) {
									// 処理なし
								} else {
									// リストに存在しない値は登録を許可しない
									errorMessagesList
											.add(EIMResource
													.getMessage(
															"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
															messageParams));
									columnNotUpdateFlag = true;// フラグが立った行は更新しない
									break TOP_LOOP;
								}
							}
						}

						// 値を設定（型変換は行わない）
						importAttribute.setTextList(splitedTextDataList);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(importAttribute.getAttributeType().getDefinitionName(),importAttribute);

						break;
					}

				} else {

					AttributeValueMaster attValueMaster = null;

					String impAttDefName = importAttribute.getAttributeType().getDefinitionName();

					switch (importAttribute.getAttributeType().getValueType()) {
					// 数値型の場合
					case LONG:
						long l = 0L;
						try {
							l = Long.parseLong(csvRowCols);

							if (INTEGER_ATTR_MAX_CHARS < csvRowCols.length()) {
								// 桁数オーバーの場合、文書ID、項目名をエラーメッセージに設定しスキップする
								errorMessagesList.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.INPUT.INTEGER.VALUE.LENGTH.OVER",
														messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}
						} catch (NumberFormatException nfex) {
							errorMessagesList.add(EIMResource.getMessage(
									"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
									messageParams));
							columnNotUpdateFlag = true;// フラグが立った行は更新しない
							break TOP_LOOP;
						}

						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						// 型変換に問題が無い場合、リストを保持しているか判定
						if (attValueMaster == null) {
							// 処理なし
						} else {

							long[] ii = attValueMaster.getInts();// リスト値取得
							boolean isContain = false;


							for (int m = 0; m < ii.length; m++) {

								if ((int) l != ii[m]) {
									// 処理なし

								} else {
									// リスト中に1つでも存在すれば登録可能
									isContain = true;
									break;
								}
							}

							if (!isContain) {
								// リストに存在しない値は登録を許可しない
								errorMessagesList
										.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
														messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}
						}

						// 値を設定
						importAttribute.setLong(l);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(impAttDefName, importAttribute);

						break;

					// DATE型の場合
					case DATE:

						Date formatDate = formatDateData(csvRowCols);

						if (formatDate == null) {
							errorMessagesList.add(EIMResource.getMessage(
									"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
									messageParams));
							columnNotUpdateFlag = true;// フラグが立った行は更新しない
							break TOP_LOOP;
						}

						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						// 型変換に問題が無い場合、リストを保持しているか判定
						if (attValueMaster == null) {
							// 処理なし
						} else {

							Date[] ii = attValueMaster.getDates();
							boolean isContain = false;

							for (int m = 0; m < ii.length; m++) {

								if ((formatDate.compareTo(ii[m])) != 0) {
									// 処理なし
								} else {
									// リスト中に1つでも存在すれば登録可能
									isContain = true;
									break;
								}
							}

							if (!isContain) {
								// リストに存在しない値は登録を許可しない
								errorMessagesList.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
														messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}

						}


						// 値を設定
						importAttribute.setDate(formatDate);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(impAttDefName, importAttribute);

						break;

					// DOUBLE型の場合
					case DOUBLE:
						double d = 0.0;
						try {
							d = Double.parseDouble(csvRowCols);

							if (REAL_NUMBER_ATTR_MAX_CHARS < csvRowCols.length()) {
								// 桁数オーバーの場合、文書ID、項目名をエラーメッセージに設定しスキップする
								errorMessagesList.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.INPUT.REAL.NUMBER.VALUE.LENGTH.OVER",
														messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}

							if (!checkInputPatern(DOUBLE_PATERN, csvRowCols)) {

								errorMessagesList.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL", messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}

						} catch (NumberFormatException nfex) {
							errorMessagesList.add(EIMResource.getMessage(
									"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL", messageParams));
							columnNotUpdateFlag = true;// フラグが立った行は更新しない
							break TOP_LOOP;
						}

						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						// 型変換に問題が無い場合、リストを保持しているか判定
						if (attValueMaster == null) {
							// 処理なし
						} else {

							double[] ii = attValueMaster.getDoubles();

							boolean isContain = false;

							for (int m = 0; m < ii.length; m++) {

								if (d != ii[m]) {

									// 処理なし
								} else {
									// リスト中に1つでも存在すれば登録可能
									isContain = true;
									break;
								}
							}

							if (!isContain) {
								// リストに存在しない値は登録を許可しない
								errorMessagesList.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
														messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}
						}

						// 値を設定
						importAttribute.setDouble(d);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(impAttDefName, importAttribute);

						break;

					// STRING型の場合
					case STRING:
						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						if (attValueMaster == null) {
							// 処理なし
						} else {

							String[] ii = attValueMaster.getStrings();

							boolean isContain = false;

							for (int m = 0; m < ii.length; m++) {
								if (csvRowCols.equals(ii[m]) == false) {
									// 処理なし
								} else {
									// リスト中に1つでも存在すれば登録可能
									isContain = true;
									break;
								}
							}

							if (!isContain) {
								// リストに存在しない値は登録を許可しない
								errorMessagesList.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
														messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}
						}

						if (STRING_ATTR_MAX_CHARS < csvRowCols.length()) {
							// 桁数オーバーの場合、文書ID、項目名をエラーメッセージに設定しスキップする
							errorMessagesList.add(EIMResource.getMessage(
													"EIM.ERROR.INCSV.INPUT.STRING.VALUE.LENGTH.OVER",
													messageParams));
							columnNotUpdateFlag = true;// フラグが立った行は更新しない
							break TOP_LOOP;
						}

						// 値を設定（型変換は行わない）
						importAttribute.setString(csvRowCols);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(impAttDefName, importAttribute);

						break;

					// TEXT型の場合
					case TEXT:
						// 属性タイプ値マスター取得。キャッシュに無い場合DBから取得する。
						attValueMaster = getAttributeVlaueMaster(at, sess);

						if (attValueMaster == null) {

							// 処理なし
						} else {

							String[] ii = attValueMaster.getTexts();

							String txt = csvRowCols;

							// 改行コード\nが存在する場合、\rに置き換える
							txt = txt.replace("\n", "\r");

							boolean isContain = false;
							for (int m = 0; m < ii.length; m++) {

								if (txt.equals(ii[m]) == false) {
									// 処理なし
								} else {
									// リスト中に1つでも存在すれば登録可能
									isContain = true;
									break;
								}

							}
							if (!isContain) {
								// リストに存在しない値は登録を許可しない
								errorMessagesList.add(EIMResource.getMessage(
														"EIM.ERROR.INCSV.ATTRIBUTE.INPUT.ILLEGAL",
														messageParams));
								columnNotUpdateFlag = true;// フラグが立った行は更新しない
								break TOP_LOOP;
							}
						}

						// 値を設定（型変換は行わない）
						importAttribute.setText(csvRowCols);
						// importAttributeList.add(importAttribute);

						// 原則オブジェクトに存在する属性のみの更新のため、必ず定義名が一致。データを置き換える
						importAttributeMap.put(importAttribute.getAttributeType().getDefinitionName(), importAttribute);

						break;
					}
				}
			}
		}

		// 更新用のattributeListをMapから取り出す
		List<AttributeDomain> importAttributeList = new ArrayList<AttributeDomain>(
				importAttributeMap.values());

		// 更新処理
		ImportAttributeResultDomain impResult;

		if (!allBlankChk) {
			Object[] params = { String.valueOf(documentId) };
			errorMessagesList.add(EIMResource.getMessage("EIM.ERROR.INCSV.SKIP.UPDATE", params));

		}

		// if(!columnNotUpdateFlag && allBlankChk != 0) {
		if (errorMessagesList.size() == 0 && !columnNotUpdateFlag) {

			documentDomain.setAttributeList(importAttributeList);

			try {
				objectService.update(documentDomain);

				// アクセス履歴作成用にIDだけいれたObjectを作成する
				EIMObject object = new EIMObject(documentDomain.getId(), null,
						null, documentDomain.getRevision(),
						documentDomain.isLatest(), null, null, null, null,
						null, null, false, false, null);

				// アクセス履歴作成
				createAccHist(object, sess);
				
				// 操作履歴作成
				createOpeHist(documentDomain, sess);

				// 検索FW更新通知 対象：ドキュメント
				AppUpdateNoticeUtils.updateNoticeInsert(object.getId(), "SEARCHFW_UPDATE_ATTR_DOCUMENT");
				
				sess.commit(); //1件ずつコミット

			} catch (EIMException e) {

				sess.rollback(); // updateでエラーが出た場合はロールバック
				// updateでエラーが出た場合はメッセージに追加して終了
				errorMessagesList.add(e.getMessage());
			}

			impResult = returnErrorResult(errorMessagesList, true);

		} else {

			impResult = returnErrorResult(errorMessagesList, false);
		}

		return impResult;
	}

	/**
	 * 指定された属性タイプに紐付く属性タイプ値マスターの一覧を取得する。
	 * 既に取得済みの場合はキャッシュから取得し、未取得の場合はDBから取得しかつ、
	 * キャッシュに登録する
	 * @param at 属性タイプドメイン
	 * @param sess セッション
	 * @return 属性タイプ値マスターのリスト
	 * @throws Exception  例外
	 */
	private static AttributeValueMaster getAttributeVlaueMaster(AttributeTypeDomain at, EIMSession sess) throws Exception {

		AttributeValueMaster attValueMaster = null;
		if(attrListCheckCache.containsKey(String.valueOf(at.getId()))){

			// 属性タイプIDがキャッシュにある場合、キャッシュからリスト値を取得
			attValueMaster = attrListCheckCache.get(String.valueOf(at.getId()));

		} else {
			// 属性タイプIDがキャッシュに無い場合、DBから取得
			attValueMaster = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess, (int) at.getId());// リスト値の取得
			// 属性タイプ値マスターをキャッシュに追加
			attrListCheckCache.put(String.valueOf(String.valueOf(at.getId())),attValueMaster);
		}

		return attValueMaster;
	}

	/**
	 * 処理結果を生成
	 *
	 * @param errorMessagesList エラーメッセージリスト
	 * @param isUpdated 取込結果
	 * @return 処理結果オブジェクト
	 */
	private static ImportAttributeResultDomain returnErrorResult(
			List<String> errorMessagesList, boolean isUpdated) {
		ImportAttributeResultDomain impResult = new ImportAttributeResultDomain();
		if (errorMessagesList != null) {
			impResult.setTempErrorMessage(errorMessagesList);
		}
		impResult.setUpdated(isUpdated);
		return impResult;
	}

	/**
	 * インポート実施前のドキュメント状態のチェックを行う チェックは全て通過させ、最後に結果をエラーメッセージのリストで返却する
	 * （一つエラーになっても後続のチェック処理を行うため、if中でreturnはしない）
	 *
	 * @param documentDomain チェック対象ドキュメント
	 * @param objectService オブジェクトサービス
	 * @param objectTypeService オブジェクトタイプサービス
	 * @param rowNum 処理対象エクセルシートの行数
	 * @return エラーメッセージのリスト
	 * @throws Exception 例外
	 */
	private static List<String> checkDocumentBeforeImport(
			ObjectDomain documentDomain, long documentId,
			ObjectService objectService, ObjectTypeService objectTypeService)
			throws Exception {

		List<String> resultList = new ArrayList<String>();
		Object[] params = { String.valueOf(documentId) };

		// 文書IDに設定されたオブジェクトが存在しない場合、エラーメッセージを追加
		if (documentDomain == null) {
			resultList.add(EIMResource.getMessage("EIM.ERROR.INCSV.OBJECT.NOTFOUND", params));
			return resultList;
		}


		if(typeCheckCache.containsKey(String.valueOf(documentDomain.getType().getId()))) {

			String value = typeCheckCache.get(String.valueOf(documentDomain.getType().getId()));

			if ("ERROR".equals(value)) {
				resultList.add(EIMResource.getMessage("EIM.ERROR.INCSV.NO.DOCUMENT", params));
				return resultList;
			}

		} else {

		// タイプがドキュメントの子であることをチェックし、ドキュメント以外の場合、エラーメッセージを追加
		ObjectTypeDomain isDocType = getRootObjType(documentDomain,
				objectTypeService);
			if (isDocType.getDefinitionName().equals(
					ConfigUtils.getByKey("OBJECT_TYPE_NAME_DOCUMENT")) == false) {
				resultList.add(EIMResource.getMessage(
						"EIM.ERROR.INCSV.NO.DOCUMENT", params));

				typeCheckCache.put(String.valueOf(documentDomain.getType().getId()), "ERROR");

			return resultList;
			} else {
				typeCheckCache.put(String.valueOf(documentDomain.getType().getId()), "OK");

			}

		}

		// 最新版で無い場合、エラーメッセージを追加
		if (!documentDomain.isLatest()) {
			resultList.add(EIMResource.getMessage("EIM.ERROR.INCSV.NO.LATEST", params));
			return resultList;
		}


		// セキュリティをチェックし、更新権限が無い場合、エラーメッセージを追加
		boolean isAccessable = false;
		isAccessable = objectService.authorized(documentDomain,
				new AccessRoleTypeDomain(EIMAccessRole.CREATE));
		if (!isAccessable) {
			// 更新権限なし
			resultList.add(EIMResource.getMessage("EIM.ERROR.INCSV.NO.PERMISSION", params));
		}

		return resultList;
	}

	/**
	 * 引数のオブジェクトのルートオブジェクトタイプドメインを取得する
	 *
	 * @param object対象オブジェクト
	 * @param objectTypeService オブジェクトタイプサービス
	 * @return ルートオブジェクトタイプドメイン
	 * @throws Exception 例外
	 */
	private static ObjectTypeDomain getRootObjType(ObjectDomain object,
			ObjectTypeService objectTypeService) throws Exception {

		// この時点でObjectTypeDomainにはparentTypeが入っていないため、オブジェクトタイプを再取得。
		ObjectTypeDomain ot = object.getType();
		ObjectTypeDomain temp = objectTypeService.getByDefinitionName(ot
				.getDefinitionName());

		if (temp != null) {
			return getRootObjType(temp);
		}
		return null;
	}

	private static ObjectTypeDomain getRootObjType(ObjectTypeDomain childType)
			throws Exception {
		ObjectTypeDomain parentType = childType.getParent();
		if (parentType != null) {
			childType = getRootObjType(parentType);
		}
		return childType;
	}

	/**
	 * 対象が上位からの引き継ぎ属性か判定する
	 *
	 * true:上位からの引き継ぎ属性である false:上位からの引き継ぎ属性ではない
	 *
	 * @param object 対象オブジェクトドメイン
	 * @param attribute 対象属性タイプドメイン
	 * @return boolean
	 * @throws Exception 例外
	 */
	private static boolean getInheritedAttList(ObjectDomain object, AttributeTypeDomain attribute) throws Exception {
		boolean bool = false;

		// 対象Objectの上位引継ぎ属性を取得
		AttributeDomain inheritedAttribute = object.getAttribute(EIMConfig.getValue("ATTR_NAME_DOCUMENT_FROM_HIGH_ATTR"));// 上位からの引継ぎ属性

		if (inheritedAttribute != null) {
			List<Long> attrIds = inheritedAttribute.getLongList();

			if (attrIds != null && attrIds.size() > 0
					&& attrIds.contains((Long) attribute.getId())) {

				bool = true;
			}
		}

		return bool;
	}

	/**
	 * 属性タイプドメインを　KEY=定義名称 VALUE=属性タイプのマップに変換
	 *
	 * @param documentObjectTypeDomain ドキュメントオブジェクトタイプドメイン
	 * @return 属性タイプマップ
	 */
	private static Map<String, AttributeTypeDomain> convertAtriibuteTypeList(
			ObjectTypeDomain documentObjectTypeDomain) {

		List<AttributeTypeDomain> documentAttrTypeList = documentObjectTypeDomain.getAttributeTypeList();

		Map<String, AttributeTypeDomain> docAttributeTypeMap = new HashMap<String, AttributeTypeDomain>();
		for (AttributeTypeDomain atDomain : documentAttrTypeList) {

			docAttributeTypeMap.put(atDomain.getDefinitionName(), atDomain);
		}

		return docAttributeTypeMap;
	}

	/**
	 * 属性ドメインリストを　KEY=属性タイプの定義名称 VALUE=属性ドメインのマップに変換
	 *
	 * @param importAttributeList
	 * @return
	 */
	private static Map<String, AttributeDomain> convertAtriibuteList(
			List<AttributeDomain> importAttributeList) {

		Map<String, AttributeDomain> attDomainMap = new HashMap<String, AttributeDomain>();

		for (AttributeDomain attDomain : importAttributeList) {

			attDomainMap.put(attDomain.getAttributeType().getDefinitionName(), attDomain);

		}

		return attDomainMap;
	}

	/**
	 * 文字列を区切り文字列で分解して返却する
	 *
	 * @param target 分解元の文字列
	 * @param spliter 区切り文字列
	 * @return 分解した文字列のリスト（Stringのリスト）
	 * @throws Exception 例外
	 */
	private static List<String> splitString(String target, String spliter)
			throws Exception {

		List<String> separatedList = new ArrayList<String>();

		if (target == null || target.equals("")) {
			separatedList.add(target);
			return separatedList;
		}

		if (spliter == null || spliter.equals("")) {
			separatedList.add(target);
			return separatedList;
		}

		String[] list = target.split(spliter);
		for (String s : list) {
			separatedList.add(s);
		}

		return separatedList;
	}

	/**
	 * アクセス履歴出力
	 *
	 * @param sess
	 *            セッション
	 * @param EIMObject
	 *            object 操作対象ドキュメント
	 * @throws Exception
	 *             例外処理
	 */
	private static void createAccHist(EIMObject object, EIMSession sess)
			throws Exception {

		// アクセス履歴作成
		AccessUtils	.createAccess(sess, object, "EIM.ACCESS.TYPE.DOCATTR.UPDATE");

	}

	/**
	 * 操作履歴出力
	 * 
	 * @param document 操作対象ドキュメント
	 * @param sess セッション
	 * @throws Exception 例外処理
	 */
	private static void createOpeHist(ObjectDomain document, EIMSession sess) throws Exception {
		// パス取得
		AttributeDomain pathAttr = document.getAttribute(EIMConfig.get("ATTR_NAME_DOCUMENT_PASS"));
		String path = pathAttr.getString();

		// 操作履歴作成
		OperationHistoryUtils.create(sess,
									AppConstant.DOCUMENT, //アプリケーション種別
									AppConstant.DOCUMENT_DOCATTR_UPDATE, //操作種別
									EIMConstant.TARGET_UPDATE, // 操作対象情報 A
									EIMConstant.OBJECT, // 操作対象種別 A
									document.getName(), // 操作対象 A
									null, // 操作対象情報 B
									null, // 操作対象種別 B
									null, // 操作対象 B
									path // 操作詳細
									);
	}

	/**
	 * 日付文字列をフォーマットして日付型で返却する。 変換できない場合はnullを返却する
	 *
	 * @return
	 * @throws Exception
	 *             例外
	 */
	private static Date formatDateData(String csvRowCols) throws Exception {

		SimpleDateFormat sdf = new SimpleDateFormat(ResourceUtils.getByKey("EIM.FORMAT.DATE"));
		sdf.setLenient(false);
		Date formatDate = null;
		// CSVのデータを-（ハイフン）区切りに変換
		String regex = "/";
		Pattern p = Pattern.compile(regex);
		Matcher m = p.matcher(csvRowCols);
		csvRowCols = m.replaceAll("-");
		try {
			formatDate = sdf.parse(csvRowCols);

		} catch (ParseException e) {

			// フォーマットエラー
			formatDate = null;
		}

		return formatDate;
	}

	/**
	 * 日付文字列をフォーマットして日付型で返却する。 変換できない場合はnullを返却する
	 *
	 * @return
	 * @throws Exception
	 *             例外
	 */
	private static List<Date> formatDateDataList(List<String> dateValues)
			throws Exception {

		List<Date> formatDateList = new ArrayList<Date>();

		for (String s : dateValues) {
			Date d = formatDateData(s);
			if (d != null) {
				formatDateList.add(d);
			} else {
				// １つでも戻りがnullの場合は、フォーマットエラー
				formatDateList = null;
				break;
			}
		}

		return formatDateList;
	}

	/**
	 * 正規表現チェック
	 *
	 * @param pattern 正規表現パターン
	 * @param str チェック対象文字列
	 * @return true：完全一致 false:不一致
	 */
	public static boolean checkInputPatern(Pattern pattern, String str) {

		Matcher m = pattern.matcher(str);
		return m.matches();
	}

}
