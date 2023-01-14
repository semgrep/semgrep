<?php

while (true) {
    switch ($this->io->ask('    <info>Discard changes [y,n,v,d,'.($update ? 's,' : '').'?]?</info> ', '?')) {
        case 'y':
            $this->discardChanges($path);
            break 2;
                                                                                                                 
        case 's':
            if (!$update) {
                //ERROR: match
                goto help;
            }
                                                                                                                 
            $this->stashChanges($path);
            break 2;
                                                                                                                 
        case 'n':
            throw new \RuntimeException('Update aborted');
                                                                                                                 
        case 'v':
            $this->io->writeError($changes);
            break;
                                                                                                                 
        case 'd':
            $this->viewDiff($path);
            break;
                                                                                                                 
        case '?':
        default:
            help :
            $this->io->writeError(array(
                '    y - discard changes and apply the '.($update ? 'update' : 'uninstall'),
                '    n - abort the '.($update ? 'update' : 'uninstall').' and let you manually clean things up',
                '    v - view modified files',
                '    d - view local modifications (diff)',
            ));
            if ($update) {
                $this->io->writeError('    s - stash changes and try to reapply them after the update');
            }
            $this->io->writeError('    ? - print help');
            break;
    }
}