package common.util;

import java.util.Date;

import common.bo.AttributeValueMaster;
import common.util.AttributeMasterUtil;

import eim.bo.EIMAttributeType;
import eim.bo.EIMResource;
import eim.bo.EIMValueType;
import eim.net.EIMSession;
import eim.util.AttributeUtils;
import eim.util.EIMConfig;
import eim.util.ObjectUtils;
import eimtest.util.TestSessionUtil;
import junit.framework.TestCase;

public class AttributeMasterUtilTest extends TestCase {

	// 作成する属性タイプの名称
	private final String testAttrTypeName1 = "AttributeMasterUtilsTest_AttrType1";

	private final String testAttrTypeName2 = "AttributeMasterUtilsTest_AttrType2";
	
	private final String testAttrTypeName3 = "AttributeMasterUtilsTest_AttrType3";

	private final String testAttrTypeName4 = "AttributeMasterUtilsTest_AttrType4";

	private final String testAttrTypeName5 = "AttributeMasterUtilsTest_AttrType5";

	private final String testAttrTypeName6 = "AttributeMasterUtilsTest_AttrType6";
	
	private final String testAttrTypeName7 = "AttributeMasterUtilsTest_AttrType7";

	private final String testAttrTypeName8 = "AttributeMasterUtilsTest_AttrType8";

	private final String testAttrTypeName9 = "AttributeMasterUtilsTest_AttrType9";

	private EIMAttributeType attrType1 = null;
	
	private EIMAttributeType attrType2 = null;

	private EIMAttributeType attrType3 = null;
	
	private EIMAttributeType attrType4 = null;

	private EIMAttributeType attrType5 = null;
	
	private EIMAttributeType attrType6 = null;

	private EIMAttributeType attrType7 = null;
	
	private EIMAttributeType attrType8 = null;

	private EIMAttributeType attrType9 = null;

	protected void setUp() throws Exception {
		super.setUp();

		/**
		 * テストデータの作成
		 */
		// セッションの生成
		EIMSession sess = TestSessionUtil.createEIMSession();

		try{
			// 属性タイプの作成
			attrType1 = AttributeUtils.createAttributeType(sess, testAttrTypeName1, EIMValueType.getTypeById(sess, EIMValueType.INTEGER), false);
			attrType2 = AttributeUtils.createAttributeType(sess, testAttrTypeName2, EIMValueType.getTypeById(sess, EIMValueType.INTEGER), true);
			attrType3 = AttributeUtils.createAttributeType(sess, testAttrTypeName3, EIMValueType.getTypeById(sess, EIMValueType.STRING), false);
			attrType4 = AttributeUtils.createAttributeType(sess, testAttrTypeName4, EIMValueType.getTypeById(sess, EIMValueType.STRING), true);
			attrType5 = AttributeUtils.createAttributeType(sess, testAttrTypeName5, EIMValueType.getTypeById(sess, EIMValueType.DATE), false);
			attrType6 = AttributeUtils.createAttributeType(sess, testAttrTypeName6, EIMValueType.getTypeById(sess, EIMValueType.DATE), true);
			attrType7 = AttributeUtils.createAttributeType(sess, testAttrTypeName7, EIMValueType.getTypeById(sess, EIMValueType.TEXT), false);
			attrType8 = AttributeUtils.createAttributeType(sess, testAttrTypeName8, EIMValueType.getTypeById(sess, EIMValueType.TEXT), true);
			attrType9 = AttributeUtils.createAttributeType(sess, testAttrTypeName9, EIMValueType.getTypeById(sess, EIMValueType.TEXT), true);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
	}

	protected void tearDown() throws Exception {
		super.tearDown();
		/**
		 * テストデータの削除
		 */
		// セッションの生成
		EIMSession sess = TestSessionUtil.createEIMSession();

		try{
			AttributeUtils.deleteAttributeType(sess, attrType1);
			AttributeUtils.deleteAttributeType(sess, attrType2);
			AttributeUtils.deleteAttributeType(sess, attrType3);
			AttributeUtils.deleteAttributeType(sess, attrType4);
			AttributeUtils.deleteAttributeType(sess, attrType5);
			AttributeUtils.deleteAttributeType(sess, attrType6);
			AttributeUtils.deleteAttributeType(sess, attrType7);
			AttributeUtils.deleteAttributeType(sess, attrType8);
			AttributeUtils.deleteAttributeType(sess, attrType9);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
	}

	/*
	 * 'common.util.AttributeMasterUtil.createAttributeValueMaster(EIMSession, EIMAttributeType)' のためのテスト・メソッド
	 */
	public void testCreateAttributeValueMaster() throws Exception{
		// セッション情報
		EIMSession sess = null;
		String errorMessage = null;
		
		AttributeValueMaster attMaster1 = null;
		AttributeValueMaster attMaster2 = null;
		AttributeValueMaster attMaster3 = null;
		AttributeValueMaster attMaster4 = null;
		AttributeValueMaster attMaster5 = null;
		AttributeValueMaster attMaster6 = null;
		AttributeValueMaster attMaster7 = null;
		AttributeValueMaster attMaster8 = null;
		
		/**
		 * 正常処理
		 * 
		 * 属性タイプ値マスターを生成できること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			attMaster2 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType2);
			attMaster3 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType3);
			attMaster4 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType4);
			attMaster5 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType5);
			attMaster6 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType6);
			attMaster7 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType7);
			attMaster8 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType8);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
		
		// DBから属性タイプ値マスターを取得できること
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType1.getId());
			attMaster2 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType2.getId());
			attMaster3 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType3.getId());
			attMaster4 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType4.getId());
			attMaster5 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType5.getId());
			attMaster6 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType6.getId());
			attMaster7 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType7.getId());
			attMaster8 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType8.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster2);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster3);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster4);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster5);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster6);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster7);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster8);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		// 属性タイプ値マスターを取得できていることを確認
		assertNotNull(attMaster1);
		assertEquals(attMaster1.getType().getId(), attrType1.getId());

		assertNotNull(attMaster2);
		assertEquals(attMaster2.getType().getId(), attrType2.getId());

		assertNotNull(attMaster3);
		assertEquals(attMaster3.getType().getId(), attrType3.getId());

		assertNotNull(attMaster4);
		assertEquals(attMaster4.getType().getId(), attrType4.getId());

		assertNotNull(attMaster5);
		assertEquals(attMaster5.getType().getId(), attrType5.getId());

		assertNotNull(attMaster6);
		assertEquals(attMaster6.getType().getId(), attrType6.getId());

		assertNotNull(attMaster7);
		assertEquals(attMaster7.getType().getId(), attrType7.getId());

		assertNotNull(attMaster8);
		assertEquals(attMaster8.getType().getId(), attrType8.getId());

		/**
		 * 異常処理
		 * 
		 * 同じ属性タイプに2つ以上のマスター定義ができないこと
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			
			// ここで例外発生
			attMaster2 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.ATTRIBUTE.MASTER.EXISTS"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * オブジェクトタイプ「属性タイプ値マスター」が存在しないとエラーになること
		 * 
		 */
		// 属性タイプ値マスターを削除する
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ「属性タイプ値マスター数値リスト」の削除
			ObjectUtils.deleteObjectType(sess, ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST")));
			// 属性の登録
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			// ロールバックしておく
			sess.rollback();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.SYSTEMERROR"));
		sess.close();
	}

	/*
	 * 'common.util.AttributeMasterUtil.deleteAttributeValueMaster(EIMSession, AttributeValueMaster)' のためのテスト・メソッド
	 */
	public void testDeleteAttributeValueMaster() throws Exception{
		// セッション情報
		EIMSession sess = null;
		String errorMessage = null;

		AttributeValueMaster attMaster1 = null;
		AttributeValueMaster attMaster2 = null;
		AttributeValueMaster attMaster3 = null;
		AttributeValueMaster attMaster4 = null;
		AttributeValueMaster attMaster5 = null;
		AttributeValueMaster attMaster6 = null;
		AttributeValueMaster attMaster7 = null;
		AttributeValueMaster attMaster8 = null;
		
		/**
		 * 事前処理
		 * 
		 * 属性タイプ値マスター生成
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			attMaster2 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType2);
			attMaster3 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType3);
			attMaster4 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType4);
			attMaster5 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType5);
			attMaster6 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType6);
			attMaster7 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType7);
			attMaster8 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType8);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
		
		/**
		 * 正常処理
		 * 
		 * 属性タイプ値マスターを削除できること
		 * 
		 */
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster2);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster3);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster4);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster5);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster6);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster7);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster8);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}

		// DBから属性タイプ値マスターを取得できないことを確認
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType1.getId());
			attMaster2 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType2.getId());
			attMaster3 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType3.getId());
			attMaster4 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType4.getId());
			attMaster5 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType5.getId());
			attMaster6 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType6.getId());
			attMaster7 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType7.getId());
			attMaster8 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType8.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		assertNull(attMaster1);
		assertNull(attMaster2);
		assertNull(attMaster3);
		assertNull(attMaster4);
		assertNull(attMaster5);
		assertNull(attMaster6);
		assertNull(attMaster7);
		assertNull(attMaster8);
		
		/**
		 * 事前処理
		 * 
		 * 属性タイプ値マスター生成
		 * 
		 */
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
		/**
		 * 異常処理
		 * 
		 * 存在しない属性タイプ値マスターを削除できないこと
		 * 
		 */
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
			// 特にエラーは発生しない
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.commit();
		} finally {
			sess.close();
		}
		// DBから属性タイプ値マスターを取得できないことを確認
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType1.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		assertNull(attMaster1);
	}

	/*
	 * 'common.util.AttributeMasterUtil.getAttributeValueMasterListById(EIMSession, int)' のためのテスト・メソッド
	 */
	public void testGetAttributeValueMasterListById() throws Exception{
		// セッション情報
		EIMSession sess = null;

		AttributeValueMaster attMaster1 = null;
		AttributeValueMaster attMaster2 = null;
		AttributeValueMaster attMaster3 = null;
		AttributeValueMaster attMaster4 = null;
		AttributeValueMaster attMaster5 = null;
		AttributeValueMaster attMaster6 = null;
		AttributeValueMaster attMaster7 = null;
		AttributeValueMaster attMaster8 = null;
		AttributeValueMaster attMaster9 = null;

		int intValue1 = 1;
		int intValue2 = 2;
		int intValue3 = 3;
		int[] multiIntValue = new int[]{intValue1,intValue2,intValue3};
		int[] tempIntValue;

		String strValue1 = "属性A";
		String strValue2 = "属性B";
		String strValue3 = "属性C";
		String strValue4 = "属性D";
		String[] multiStrValue = new String[]{strValue1,strValue2,strValue3,strValue4};
		String[] tempStrValue;
		
		Date dateValue1 = new Date(2000,1,1);
		Date dateValue2 = new Date(2000,1,2);
		Date dateValue3 = new Date(2000,1,3);
		Date dateValue4 = new Date(2000,1,4);
		Date[] multiDateValue = new Date[]{dateValue1,dateValue2,dateValue3,dateValue4};
		Date[] tempDateValue;
		
		/**
		 * 事前処理
		 * 
		 * 属性タイプ値マスター生成
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ値マスターの生成
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			attMaster2 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType2);
			attMaster3 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType3);
			attMaster4 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType4);
			attMaster5 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType5);
			attMaster6 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType6);
			attMaster7 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType7);
			attMaster8 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType8);
			//attrType9には属性タイプ値マスターを登録しない

			// マスター値を登録
			AttributeMasterUtil.setIntAttributeValues(sess, attMaster1, multiIntValue);
			AttributeMasterUtil.setIntAttributeValues(sess, attMaster2, multiIntValue);
			AttributeMasterUtil.setStrAttributeValues(sess, attMaster3, multiStrValue);
			AttributeMasterUtil.setStrAttributeValues(sess, attMaster4, multiStrValue);
			AttributeMasterUtil.setDateAttributeValues(sess, attMaster5, multiDateValue);
			AttributeMasterUtil.setDateAttributeValues(sess, attMaster6, multiDateValue);
			AttributeMasterUtil.setTextAttributeValues(sess, attMaster7, multiStrValue);
			AttributeMasterUtil.setTextAttributeValues(sess, attMaster8, multiStrValue);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
		/**
		 * 正常処理
		 * 
		 * 属性タイプ値マスターを取得できること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType1.getId());
			attMaster2 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType2.getId());
			attMaster3 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType3.getId());
			attMaster4 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType4.getId());
			attMaster5 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType5.getId());
			attMaster6 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType6.getId());
			attMaster7 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType7.getId());
			attMaster8 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType8.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		// 属性タイプ値マスターを取得できていることを確認
		assertNotNull(attMaster1);
		assertEquals(attMaster1.getType().getId(), attrType1.getId());
		tempIntValue = attMaster1.getInts();
		assertEquals(tempIntValue.length, multiIntValue.length);
		for(int i = 0; i < tempIntValue.length ; i++)
		{
			assertEquals(tempIntValue[i], multiIntValue[i]);
		}

		assertNotNull(attMaster2);
		assertEquals(attMaster2.getType().getId(), attrType2.getId());
		tempIntValue = attMaster2.getInts();
		assertEquals(tempIntValue.length, multiIntValue.length);
		for(int i = 0; i < tempIntValue.length ; i++)
		{
			assertEquals(tempIntValue[i], multiIntValue[i]);
		}

		assertNotNull(attMaster3);
		assertEquals(attMaster3.getType().getId(), attrType3.getId());
		tempStrValue = attMaster3.getStrings();
		assertEquals(tempStrValue.length, multiStrValue.length);
		for(int i = 0; i < tempStrValue.length ; i++)
		{
			assertEquals(tempStrValue[i], multiStrValue[i]);
		}

		assertNotNull(attMaster4);
		assertEquals(attMaster4.getType().getId(), attrType4.getId());
		tempStrValue = attMaster4.getStrings();
		assertEquals(tempStrValue.length, multiStrValue.length);
		for(int i = 0; i < tempStrValue.length ; i++)
		{
			assertEquals(tempStrValue[i], multiStrValue[i]);
		}

		assertNotNull(attMaster5);
		assertEquals(attMaster5.getType().getId(), attrType5.getId());
		tempDateValue = attMaster5.getDates();
		assertEquals(tempDateValue.length, multiDateValue.length);
		for(int i = 0; i < tempDateValue.length ; i++)
		{
			assertEquals(tempDateValue[i], multiDateValue[i]);
		}

		assertNotNull(attMaster6);
		assertEquals(attMaster6.getType().getId(), attrType6.getId());
		tempDateValue = attMaster6.getDates();
		assertEquals(tempDateValue.length, multiDateValue.length);
		for(int i = 0; i < tempDateValue.length ; i++)
		{
			assertEquals(tempDateValue[i], multiDateValue[i]);
		}

		assertNotNull(attMaster7);
		assertEquals(attMaster7.getType().getId(), attrType7.getId());
		tempStrValue = attMaster7.getTexts();
		assertEquals(tempStrValue.length, multiStrValue.length);
		for(int i = 0; i < tempStrValue.length ; i++)
		{
			assertEquals(tempStrValue[i], multiStrValue[i]);
		}

		assertNotNull(attMaster8);
		assertEquals(attMaster8.getType().getId(), attrType8.getId());
		tempStrValue = attMaster8.getTexts();
		assertEquals(tempStrValue.length, multiStrValue.length);
		for(int i = 0; i < tempStrValue.length ; i++)
		{
			assertEquals(tempStrValue[i], multiStrValue[i]);
		}

		/**
		 * 異常処理
		 * 
		 * 存在しない属性タイプ値マスターを取得できないこと
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster9 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType9.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		// NULLが帰ってくることを確認
		assertNull(attMaster9);

		/**
		 * 事後処理
		 * 
		 * 属性タイプ値マスター削除
		 * 
		 */
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster2);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster3);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster4);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster5);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster6);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster7);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster8);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
	}

	/*
	 * 'common.util.AttributeMasterUtil.setIntAttributeValues(EIMSession, AttributeValueMaster, int[])' のためのテスト・メソッド
	 */
	public void testSetIntAttributeValues() throws Exception{
		// セッション情報
		EIMSession sess = null;
		String errorMessage = null;

		AttributeValueMaster attMaster1 = null;
		AttributeValueMaster attMaster2 = null;
		AttributeValueMaster attMaster3 = null;
		AttributeValueMaster attMaster5 = null;
		AttributeValueMaster attMaster7 = null;

		int intValue1 = 1;
		int intValue2 = 2;
		int intValue3 = 3;
		int[] multiIntValue = new int[]{intValue1,intValue2,intValue3};
		int[] tempIntValue;

		
		/**
		 * 事前処理
		 * 
		 * 属性タイプ値マスター生成
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ値マスターの生成
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			attMaster2 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType2);
			attMaster3 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType3);
			attMaster5 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType5);
			attMaster7 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType7);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}

		/**
		 * 正常処理
		 * 
		 * 数値型の属性タイプ値設定ができること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 単一値の属性にマスター値を登録
			AttributeMasterUtil.setIntAttributeValues(sess, attMaster1, multiIntValue);
			// 複数値の属性にマスター値を登録
			AttributeMasterUtil.setIntAttributeValues(sess, attMaster2, multiIntValue);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
		// 属性タイプ値マスターの取得
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster1 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType1.getId());
			attMaster2 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType2.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		// 数値型の属性タイプ値マスターを設定できていることを確認
		assertNotNull(attMaster1);
		assertEquals(attMaster1.getType().getId(), attrType1.getId());
		tempIntValue = attMaster1.getInts();
		assertEquals(tempIntValue.length, multiIntValue.length);
		for(int i = 0; i < tempIntValue.length ; i++)
		{
			assertEquals(tempIntValue[i], multiIntValue[i]);
		}

		assertNotNull(attMaster2);
		assertEquals(attMaster2.getType().getId(), attrType2.getId());
		tempIntValue = attMaster2.getInts();
		assertEquals(tempIntValue.length, multiIntValue.length);
		for(int i = 0; i < tempIntValue.length ; i++)
		{
			assertEquals(tempIntValue[i], multiIntValue[i]);
		}

		/**
		 * 異常処理
		 * 
		 * 文字列型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setIntAttributeValues(sess,attMaster3,multiIntValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 日付型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setIntAttributeValues(sess,attMaster5,multiIntValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * テキスト型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setIntAttributeValues(sess,attMaster7,multiIntValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();


		/**
		 * 事後処理
		 * 
		 * 属性タイプ値マスター削除
		 * 
		 */
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster2);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster3);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster5);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster7);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}

		/**
		 * 異常処理
		 * 
		 * 存在しない属性タイプ値マスターに設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性にマスター値を登録
			AttributeMasterUtil.setIntAttributeValues(sess, attMaster1, multiIntValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 属性タイプ「属性タイプ値マスター数値リスト」が存在しないとエラーになること
		 * 
		 */
		// 属性タイプ値マスターを削除する
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ「属性タイプ値マスター数値リスト」の削除
			ObjectUtils.deleteObjectType(sess, ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST")));
			AttributeUtils.deleteAttributeType(sess, AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_NUM_LIST")));
			ObjectUtils.createObjectType(sess,EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), null);
			// 属性の登録
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			// 属性にマスター値を登録
			AttributeMasterUtil.setIntAttributeValues(sess, attMaster1, multiIntValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			// ロールバックしておく
			sess.rollback();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.SYSTEMERROR"));
		sess.close();
	}

	/*
	 * 'common.util.AttributeMasterUtil.setStrAttributeValues(EIMSession, AttributeValueMaster, String[])' のためのテスト・メソッド
	 */
	public void testSetStrAttributeValues() throws Exception {
		// セッション情報
		EIMSession sess = null;
		String errorMessage = null;

		AttributeValueMaster attMaster1 = null;
		AttributeValueMaster attMaster3 = null;
		AttributeValueMaster attMaster4 = null;
		AttributeValueMaster attMaster5 = null;
		AttributeValueMaster attMaster7 = null;

		String strValue1 = "属性A";
		String strValue2 = "属性B";
		String strValue3 = "属性C";
		String strValue4 = "属性D";
		String[] multiStrValue = new String[]{strValue1,strValue2,strValue3,strValue4};
		String[] tempStrValue;
		
		/**
		 * 事前処理
		 * 
		 * 属性タイプ値マスター生成
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ値マスターの生成
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			attMaster3 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType3);
			attMaster4 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType4);
			attMaster5 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType5);
			attMaster7 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType7);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}

		/**
		 * 正常処理
		 * 
		 * 文字列型の属性タイプ値設定ができること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 単一値の属性にマスター値を登録
			AttributeMasterUtil.setStrAttributeValues(sess, attMaster3, multiStrValue);
			// 複数値の属性にマスター値を登録
			AttributeMasterUtil.setStrAttributeValues(sess, attMaster4, multiStrValue);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
		// 属性タイプ値マスターの取得
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster3 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType3.getId());
			attMaster4 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType4.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		// 文字列型の属性タイプ値マスターを設定できていることを確認
		assertNotNull(attMaster3);
		assertEquals(attMaster3.getType().getId(), attrType3.getId());
		tempStrValue = attMaster3.getStrings();
		assertEquals(tempStrValue.length, multiStrValue.length);
		for(int i = 0; i < tempStrValue.length ; i++)
		{
			assertEquals(tempStrValue[i], multiStrValue[i]);
		}

		assertNotNull(attMaster4);
		assertEquals(attMaster4.getType().getId(), attrType4.getId());
		tempStrValue = attMaster4.getStrings();
		assertEquals(tempStrValue.length, multiStrValue.length);
		for(int i = 0; i < tempStrValue.length ; i++)
		{
			assertEquals(tempStrValue[i], multiStrValue[i]);
		}

		/**
		 * 異常処理
		 * 
		 * 数値型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setStrAttributeValues(sess,attMaster1,multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 日付型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setStrAttributeValues(sess,attMaster5,multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * テキスト型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setStrAttributeValues(sess,attMaster7,multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();


		/**
		 * 事後処理
		 * 
		 * 属性タイプ値マスター削除
		 * 
		 */
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster3);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster4);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster5);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster7);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}

		/**
		 * 異常処理
		 * 
		 * 存在しない属性タイプ値マスターに設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性にマスター値を登録
			AttributeMasterUtil.setStrAttributeValues(sess, attMaster3, multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 属性タイプ「属性タイプ値マスター文字列値リスト」が存在しないとエラーになること
		 * 
		 */
		// 属性タイプ値マスターを削除する
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ「属性タイプ値マスター文字列値リスト」の削除
			ObjectUtils.deleteObjectType(sess, ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST")));
			AttributeUtils.deleteAttributeType(sess, AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_STR_LIST")));
			ObjectUtils.createObjectType(sess,EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), null);
			// 属性の登録
			attMaster3 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType3);
			// 属性にマスター値を登録
			AttributeMasterUtil.setStrAttributeValues(sess, attMaster3, multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			// ロールバックしておく
			sess.rollback();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.SYSTEMERROR"));
		sess.close();
	}

	/*
	 * 'common.util.AttributeMasterUtil.setDateAttributeValues(EIMSession, AttributeValueMaster, Date[])' のためのテスト・メソッド
	 */
	public void testSetDateAttributeValues() throws Exception{
		// セッション情報
		EIMSession sess = null;
		String errorMessage = null;

		AttributeValueMaster attMaster1 = null;
		AttributeValueMaster attMaster3 = null;
		AttributeValueMaster attMaster5 = null;
		AttributeValueMaster attMaster6 = null;
		AttributeValueMaster attMaster7 = null;

		Date dateValue1 = new Date(2000,1,1);
		Date dateValue2 = new Date(2000,1,2);
		Date dateValue3 = new Date(2000,1,3);
		Date dateValue4 = new Date(2000,1,4);
		Date[] multiDateValue = new Date[]{dateValue1,dateValue2,dateValue3,dateValue4};
		Date[] tempDateValue;
		
		/**
		 * 事前処理
		 * 
		 * 属性タイプ値マスター生成
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ値マスターの生成
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			attMaster3 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType3);
			attMaster5 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType5);
			attMaster6 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType6);
			attMaster7 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType7);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}

		/**
		 * 正常処理
		 * 
		 * 日付型の属性タイプ値設定ができること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 単一値の属性にマスター値を登録
			AttributeMasterUtil.setDateAttributeValues(sess, attMaster5, multiDateValue);
			// 複数値の属性にマスター値を登録
			AttributeMasterUtil.setDateAttributeValues(sess, attMaster6, multiDateValue);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
		// 属性タイプ値マスターの取得
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster5 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType5.getId());
			attMaster6 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType6.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		// 日付型の属性タイプ値マスターを設定できていることを確認
		assertNotNull(attMaster5);
		assertEquals(attMaster5.getType().getId(), attrType5.getId());
		tempDateValue = attMaster5.getDates();
		assertEquals(tempDateValue.length, multiDateValue.length);
		for(int i = 0; i < tempDateValue.length ; i++)
		{
			assertEquals(tempDateValue[i], multiDateValue[i]);
		}

		assertNotNull(attMaster6);
		assertEquals(attMaster6.getType().getId(), attrType6.getId());
		tempDateValue = attMaster6.getDates();
		assertEquals(tempDateValue.length, multiDateValue.length);
		for(int i = 0; i < tempDateValue.length ; i++)
		{
			assertEquals(tempDateValue[i], multiDateValue[i]);
		}

		/**
		 * 異常処理
		 * 
		 * 数値型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setDateAttributeValues(sess,attMaster1,multiDateValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 文字列型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setDateAttributeValues(sess,attMaster3,multiDateValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * テキスト型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setDateAttributeValues(sess,attMaster7,multiDateValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();


		/**
		 * 事後処理
		 * 
		 * 属性タイプ値マスター削除
		 * 
		 */
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster3);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster5);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster6);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster7);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}

		/**
		 * 異常処理
		 * 
		 * 存在しない属性タイプ値マスターに設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性にマスター値を登録
			AttributeMasterUtil.setDateAttributeValues(sess, attMaster5, multiDateValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 属性タイプ「属性タイプ値マスター日付値リスト」が存在しないとエラーになること
		 * 
		 */
		// 属性タイプ値マスターを削除する
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ「属性タイプ値マスター日付値リスト」の削除
			ObjectUtils.deleteObjectType(sess, ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST")));
			AttributeUtils.deleteAttributeType(sess, AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_DATE_LIST")));
			ObjectUtils.createObjectType(sess,EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), null);
			// 属性の登録
			attMaster5 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType5);
			// 属性にマスター値を登録
			AttributeMasterUtil.setDateAttributeValues(sess, attMaster5, multiDateValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			// ロールバックしておく
			sess.rollback();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.SYSTEMERROR"));
		sess.close();
	}

	/*
	 * 'common.util.AttributeMasterUtil.setTextAttributeValues(EIMSession, AttributeValueMaster, String[])' のためのテスト・メソッド
	 */
	public void testSetTextAttributeValues() throws Exception {
		// セッション情報
		EIMSession sess = null;
		String errorMessage = null;

		AttributeValueMaster attMaster1 = null;
		AttributeValueMaster attMaster3 = null;
		AttributeValueMaster attMaster5 = null;
		AttributeValueMaster attMaster7 = null;
		AttributeValueMaster attMaster8 = null;

		String strValue1 = "属性A";
		String strValue2 = "属性B";
		String strValue3 = "属性C";
		String strValue4 = "属性D";
		String[] multiStrValue = new String[]{strValue1,strValue2,strValue3,strValue4};
		String[] tempStrValue;
		
		/**
		 * 事前処理
		 * 
		 * 属性タイプ値マスター生成
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ値マスターの生成
			attMaster1 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType1);
			attMaster3 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType3);
			attMaster5 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType5);
			attMaster7 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType7);
			attMaster8 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType8);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}

		/**
		 * 正常処理
		 * 
		 * テキスト型の属性タイプ値設定ができること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 単一値の属性にマスター値を登録
			AttributeMasterUtil.setTextAttributeValues(sess, attMaster7, multiStrValue);
			// 複数値の属性にマスター値を登録
			AttributeMasterUtil.setTextAttributeValues(sess, attMaster8, multiStrValue);
		} catch (Exception e) {
			e.printStackTrace();
			sess.rollback();
		} finally {
			sess.close();
		}
		// 属性タイプ値マスターの取得
		sess = TestSessionUtil.createEIMSession();
		try {
			attMaster7 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType7.getId());
			attMaster8 = AttributeMasterUtil.getAttributeValueMasterByAttTypeId(sess,attrType8.getId());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}
		
		// 数値型の属性タイプ値マスターを設定できていることを確認
		assertNotNull(attMaster7);
		assertEquals(attMaster7.getType().getId(), attrType7.getId());
		tempStrValue = attMaster7.getTexts();
		assertEquals(tempStrValue.length, multiStrValue.length);
		for(int i = 0; i < tempStrValue.length ; i++)
		{
			assertEquals(tempStrValue[i], multiStrValue[i]);
		}

		assertNotNull(attMaster8);
		assertEquals(attMaster8.getType().getId(), attrType8.getId());
		tempStrValue = attMaster8.getTexts();
		assertEquals(tempStrValue.length, multiStrValue.length);
		for(int i = 0; i < tempStrValue.length ; i++)
		{
			assertEquals(tempStrValue[i], multiStrValue[i]);
		}

		/**
		 * 異常処理
		 * 
		 * 文字列型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setTextAttributeValues(sess,attMaster1,multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 数値型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setTextAttributeValues(sess,attMaster1,multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 日付型の属性に設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.setTextAttributeValues(sess,attMaster5,multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
			sess.rollback();
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.INVALID.ATTRIBUTE.VALUE.TYPE"));
		sess.close();


		/**
		 * 事後処理
		 * 
		 * 属性タイプ値マスター削除
		 * 
		 */
		// DBから属性タイプ値マスターを削除
		sess = TestSessionUtil.createEIMSession();
		try {
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster1);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster3);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster5);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster7);
			AttributeMasterUtil.deleteAttributeValueMaster(sess,attMaster8);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			sess.close();
		}

		/**
		 * 異常処理
		 * 
		 * 存在しない属性タイプ値マスターに設定するとエラーになること
		 * 
		 */
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性にマスター値を登録
			AttributeMasterUtil.setTextAttributeValues(sess, attMaster7, multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			sess.close();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.COMMON.API.NO.ATTRIBUTE.MASTER.OBJECT"));
		sess.close();

		/**
		 * 異常処理
		 * 
		 * 属性タイプ「属性タイプ値マスターテキスト値リスト」が存在しないとエラーになること
		 * 
		 */
		// 属性タイプ値マスターを削除する
		sess = TestSessionUtil.createEIMSession();
		try {
			// 属性タイプ「属性タイプ値マスターテキスト値リスト」の削除
			ObjectUtils.deleteObjectType(sess, ObjectUtils.getObjectTypeByName(sess, EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST")));
			AttributeUtils.deleteAttributeType(sess, AttributeUtils.getAttributeTypeByName(sess,EIMConfig.get("ATTR_NAME_ATTRMST_ATTR_TEXT_LIST")));
			ObjectUtils.createObjectType(sess,EIMConfig.get("OBJECT_TYPE_NAME_ATTRMST"), null);
			// 属性の登録
			attMaster7 = AttributeMasterUtil.createAttributeValueMaster(sess, attrType7);
			// 属性にマスター値を登録
			AttributeMasterUtil.setTextAttributeValues(sess, attMaster7, multiStrValue);
		} catch (Exception e) {
			errorMessage = e.getMessage();
			System.out.println(errorMessage);
		} finally {
			// ロールバックしておく
			sess.rollback();
		}
		// エラーメッセージが正しいことを確認
		sess = TestSessionUtil.createEIMSession();
		assertEquals(errorMessage, EIMResource.getMessage(sess,"EIM.ERROR.SYSTEMERROR"));
		sess.close();
	}

}
