function C = job_longp(idx)
        try
            fprintf('\nHello! This is job number %d. \n\n', idx)
            cd('/scratch/cs/csb/projects/LGPR/gpstuff-master/LonGP')  % change to LonGP directory
            run('../startup.m')    % load GPstuff environment
            parentDir = ['/scratch/cs/csb/projects/LGPR/fall_2019/sim_longp/longp/results/res', num2str(idx)];
            lonGP(parentDir,1)   % run lonGP
        catch error
            disp(getReport(error))
            exit(1)
        end
end
