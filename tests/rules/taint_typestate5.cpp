void lambda_in_loop() {
	playback_list.erase(foo, [](AudioStreamPlaybackListNode *p) {
		// ok: double-delete
		delete p;
	});

	for (AudioStreamPlaybackListNode *playback : playback_list) {
        // `p` is an parameter to the lambda expression. It does NOT refer
        // to the same object on each loop iteration
		playback_list.erase(playback, [](AudioStreamPlaybackListNode *p) {
			// ok: double-delete
			delete p;
		});
    }
}
