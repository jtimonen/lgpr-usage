function C = main_longp(idx)
        try
            fprintf('\nHello! This is job number %d. \n\n', idx)
            cd('/scratch/work/timonej3/GPstuff-4.7/LonGP')  % change to LonGP directory
            run('../startup.m')    % load GPstuff environment
            parentDir = ['/scratch/cs/csb/projects/LGPR/2019_summer/02_longp/longp/results/res', num2str(idx)];
            lonGP(parentDir,1)   % run lonGP
        catch error
            disp(getReport(error))
            exit(1)
        end
end
