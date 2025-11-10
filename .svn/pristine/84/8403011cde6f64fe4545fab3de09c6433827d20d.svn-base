/**
 * check_scr_Controller
 */
var check_scr_Controller = function () {
	/*****************************************************************
	 * 宣言
	 ****************************************************************/
	function F() {};
	F.prototype = prototypeController;
	var f = new F();
	
	// ビュー
	const viewName = "check_scr_View";
	const viewId = "#" + viewName;
	var view = null;
	// モデル
	const model = check_scr_Model;
	
	
	/*****************************************************************
	 * 初期処理
	 ****************************************************************/
	jQuery(document).on('pageinit', viewId, function(ev) {
		
		/**
		 * 変数とセレクタの関連付け
		 */
		// ビュー
		view = $(viewId);
		
		// ボタン
		
		
		/**
		 * initialize
		 */
		initialize();
		
		
		/*****************************************************************
		 * イベントハンドラ
		 ****************************************************************/
		/**
		 * 「EVENT_ORIENTATIONCHANGE_PORTRAIT」イベントハンドラ
		 * 縦向きになった場合
		 * このイベントハンドラは全ての画面の回転イベントで呼び出されてしまう。
		 * 現在ページの判定処理を入れて対応する
		 */
		$(this).on('EVENT_ORIENTATIONCHANGE_PORTRAIT', function(e) {
			$('#check_scr_View-displayer').css('width','17%');
		});
		
		/**
		 * 「EVENT_ORIENTATIONCHANGE_LANDSCAPE」イベントハンドラ
		 * 横向きになった場合
		 * このイベントハンドラは全ての画面の回転イベントで呼び出されてしまう。
		 * 現在ページの判定処理を入れて対応する
		 */
		
		$(this).on('EVENT_ORIENTATIONCHANGE_LANDSCAPE', function(e) {
			$('#check_scr_View-displayer').css('width','12%');
		});

		/**
		 * ボタン クリックイベントハンドラ
		 */
		jQuery(ev.target).on('vclick', sPrevBtn.selector, function() {
			$(this).removeClass('ui-btn-active');
			changeDate(-1);
			init_cache();
			slider.trigger("change");
		});

		
		/**
		 * 「success」イベントハンドラ
		 */
		view.on('success', function(e, data) {
			;
		});
		
		/**
		 * 「error」イベントハンドラ
		 */
		view.on('error', function(e, data) {
			;
		});
		
		/**
		 * 「getXXXXXSuccess」イベントハンドラ
		 */
		view.on('getXXXXXSuccess', function(e, data) {
			insert_maintenance_values(JSON.parse(data));
		});
		
		/**
		 * 「getXXXXXError」イベントハンドラ
		 */
		view.on('getXXXXXError', function(e, data) {;
		});				
		
	});
	
	/*****************************************************************
	 * ページ表示処理
	 ****************************************************************/
	jQuery(document).on('pageinit', viewId, function(ev) {
		;
	});
	
	jQuery(document).on('pagebeforeshow', viewId, function(ev) {

		f.setTitle("XXXXXXXXXXXXXXXXXXXX");
		
		
		// nullの場合、戻るボタンで画面遷移していないのでページの初期表示処理前にカレンダーをクリアする。
		if(($.mobile.urlHistory.getNext() == null)&&(!initialized_ok)){
		}
		
	});
	
	jQuery(document).on('pageshow', viewId, function(ev) {
		
		
		// nullの場合、戻るボタンで画面遷移していないのでページの表示処理を行う。
		if(($.mobile.urlHistory.getNext() == null) || logout_cancel || first_call){

		};
		
	});
	
	/*****************************************************************
	 * 関数定義
	 ****************************************************************/
	/**
	 * initialize
	 */
	function initialize () {
		;
	}
	
	/**
	 * パネル表示
	 */
	function dispGrid () {
		var grid_str= '<table class="class-table-display-information"><thead><tr height="5px"><th width="4%"></th><th width="6%">1</th><th width="6%">2</th><th width="6%">3</th><th width="6%">4</th><th width="6%">5</th><th width="6%">6</th><th width="6%">7</th><th width="6%">8</th><th width="6%">9</th><th width="6%">10</th><th width="6%">11</th><th width="6%">12</th><th width="6%">13</th><th width="6%">14</th><th width="6%">15</th><th width="6%">16</th></tr></thead><tbody>';
		for (var i=1; i<17; i++){
			grid_str += '<tr height="23px">';
			for (var j=0; j<17; j++){
				if (j==0){
					grid_str += '<td align="center">';
					grid_str += val_to_row(i);
					grid_str += '</td>';
				}
				else{
					grid_str += '<td id="cell_data_' + i + '_' + j + '" style="background: -webkit-linear-gradient(top, #FFFFDD 0%, #FFFE98 100%);" align="center">';
					grid_str += '<a class="grid_element" href="#selectPowerPlantGridView" data-inline="true" data-transition="pop" class="ui-btn-right jqm-home class-btn-logout-login-user-name" style="text-decoration: none; color: black;" id="' + 'data_' + i + '_' + j +'">';
					grid_str += '&nbsp;&nbsp;&nbsp;&nbsp;';
					grid_str += '</a>';
					grid_str += '</td>';
				};
			}
			grid_str += '</tr>';
		}
		grid_str += '</tbody></table>';
		sPlantGrid.empty().append(grid_str);
	}
		
	
	/**
	 * getValue
	 */
	function getValue () {
		return null;
	}
	
	/**
	 * setValue
	 */
	function setValue (data) {
		;
	}
	
	/**
	 * getParameter
	 */
	function getParameter () {
		var param = window.localStorage.getItem(viewName);
		return JSON.parse(param);
	}
	
	/**
	 * setParameter
	 */
	function setParameter (data) {
		window.localStorage.setItem(viewName, JSON.stringify(data));
	}
		
	/**
	 * getViewId
	 */
	function getViewId (controller) {
		return viewId;
	}
	
	/*****************************************************************
	 * 公開設定
	 ****************************************************************/
	return {
		getParameter : getParameter,
		setParameter : setParameter,
		getViewId : getViewId,
	};
	
}();
